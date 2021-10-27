# ledger-state

This tool allows loading the ledger state from binary encoded CBOR format into
sqlite database in order to perform various analysis. In particular benchmark
the memory overhead.

## Dumping LedgerState

In order to be able to use the tool we need to get ahold of current ledger
state. For this we need to start a cardano node and wait for it to sync.

```haskell
$ export CARDANO_DATA="${HOME}/iohk/chain/mainnet"
$ mkdir -p "${CARDANO_DATA}"/db
$ cd "${CARDANO_DATA}"
```

Download all the [mainnet related config files](https://developers.cardano.org/docs/get-started/running-cardano/#mainnet--production):
```
curl -O -J https://hydra.iohk.io/build/7370192/download/1/mainnet-config.json
curl -O -J https://hydra.iohk.io/build/7370192/download/1/mainnet-byron-genesis.json
curl -O -J https://hydra.iohk.io/build/7370192/download/1/mainnet-shelley-genesis.json
curl -O -J https://hydra.iohk.io/build/7370192/download/1/mainnet-alonzo-genesis.json
curl -O -J https://hydra.iohk.io/build/7370192/download/1/mainnet-topology.json
```

Start the node and wait for it to fully sync

```
$ export CARDANO_NODE_SOCKET_PATH="${CARDANO_DATA}/db/node.socket"
$ cardano-node run
  --topology "${CARDANO_DATA}/mainnet-topology.json" \
  --database-path "${CARDANO_DATA}/db" \
  --socket-path "${CARDANO_NODE_SOCKET_PATH}" \
  --host-addr 127.0.0.1 \
  --port 3001 \
  --config "${CARDANO_DATA}/mainnet-config.json" &
```

Dump the ledger state and focus back onto the node:

```shell
$ cardano-cli query ledger-state --mainnet --out-file "${CARDANO_DATA}/ledger-state.bin"
$ fg
```
Hit Ctr-C to stop the node

## Populate sqlite db

```shell
$ cabal run ledger-state --new-epoch-state-cbor="${CARDANO_DATA}/ledger-state.bin" --new-epoch-state-sqlite="${CARDANO_DATA}/ledger-state.sqlite"
```

## Running benchmarks

```shell
$ cabal bench ledger-state --benchmark-options="--new-epoch-state-cbor=\"${CARDANO_DATA}/ledger-state.bin\" --new-epoch-state-sqlite=\"${CARDANO_DATA}/ledger-state.sqlite\""
```
