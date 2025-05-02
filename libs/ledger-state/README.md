# ledger-state

This tool allows loading the ledger state from binary encoded CBOR format into
sqlite database in order to perform various analysis. In particular benchmark
the memory overhead.

## Dumping LedgerState

In order to be able to use the tool we need to get ahold of current ledger
state. For this we need to start a cardano node and wait for it to sync.

```shell
$ export CARDANO_DATA=${HOME}/iohk/chain/mainnet
$ mkdir -p "${CARDANO_DATA}/db"
$ cd "${CARDANO_DATA}"
```

Download all the [mainnet related config files](https://developers.cardano.org/docs/get-started/cardano-node/running-cardano/#configuration-files):

```shell
curl -O -J https://book.play.dev.cardano.org/environments/mainnet/config.json
curl -O -J https://book.play.dev.cardano.org/environments/mainnet/db-sync-config.json
curl -O -J https://book.play.dev.cardano.org/environments/mainnet/submit-api-config.json
curl -O -J https://book.play.dev.cardano.org/environments/mainnet/topology.json
curl -O -J https://book.play.dev.cardano.org/environments/mainnet/byron-genesis.json
curl -O -J https://book.play.dev.cardano.org/environments/mainnet/shelley-genesis.json
curl -O -J https://book.play.dev.cardano.org/environments/mainnet/alonzo-genesis.json
curl -O -J https://book.play.dev.cardano.org/environments/mainnet/conway-genesis.json
```

Download or build copies of `cardano-node` and `cardano-cli`. A convenient way of doing this is to download the assets from one of the `cardano-node` [releases](https://github.com/IntersectMBO/cardano-node/releases) on GitHub. The Linux executables are statically linked, so will run on any system.

Download a snapshot of the node db using [mithril](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node/#bootstrap-a-cardano-node-from-a-testnet-mithril-cardano-db-snapshot). This will greatly speed up the process of syncing the node. There's a convenient nix-based script for doing it in `scripts/mithril-download.sh`:

```shell
$ scripts/mithril-download.sh -d "${CARDANO_DATA}/db" mainnet
```

Note that you will need to get a snapshot that's compatible with the version of `cardano-node` you're using.

Start the node and wait for it to fully sync

```shell
$ export CARDANO_NODE_SOCKET_PATH="${CARDANO_DATA}/db/node.socket"
$ cardano-node run \
    --topology "${CARDANO_DATA}/topology.json" \
    --config "${CARDANO_DATA}/config.json" \
    --database-path "${CARDANO_DATA}/db" \
    --socket-path "${CARDANO_NODE_SOCKET_PATH}" \
    --host-addr 0.0.0.0 \
    --port 3001 &
```

Dump the ledger state:

```shell
$ cardano-cli query ledger-state --mainnet \
    --socket-path "${CARDANO_NODE_SOCKET_PATH}" \
    --out-file "${CARDANO_DATA}/new-epoch-state.bin"
```

Bring the node back into the foreground and use Ctrl-C to stop it:

```shell
$ fg
$ ^C
```

## Populate sqlite db from `NewEpochState` file

```shell
$ cabal run -- ledger-state:ledger-state \
    --new-epoch-state-cbor="${CARDANO_DATA}/new-epoch-state.bin" \
    --sqlite-db="${CARDANO_DATA}/epoch-state.sqlite"
```

## Create `EpochState` file from sqlite db

```shell
$ cabal run -- ledger-state:ledger-state \
    --epoch-state-cbor="${CARDANO_DATA}/epoch-state.bin" \
    --sqlite-db="${CARDANO_DATA}/epoch-state.sqlite"
```

## Running benchmarks

### Memory

```shell
$ cabal build ledger-state:memory
$ cabal bench ledger-state:memory -v0 \
    --benchmark-option="--new-epoch-state-cbor=${CARDANO_DATA}/new-epoch-state.bin" \
    --benchmark-option="--epoch-state-cbor=${CARDANO_DATA}/epoch-state.bin" \
    --benchmark-option="--sqlite-db=${CARDANO_DATA}/epoch-state.sqlite" |
  tee ledger-state:memory.txt
```
### Performance

Performance benchmarks need an actual mainnet ledger state and genesis config
file to run properly. It is not possible to add extra arguments to criterion cli
menu, therefore paths to those files must be supplied as environment variables.

```shell
$ export BENCH_GENESIS_PATH=${CARDANO_DATA}/shelley-genesis.json
$ export BENCH_LEDGER_STATE_PATH=${CARDANO_DATA}/new-epoch-state.bin
$ cabal bench ledger-state:performance --benchmark-option=--csv=ledger-state:performance.csv
```

The csv file will be saved in the `libs/ledger-state` directory.

Since the `performance` benchmark uses only `new-epoch-state.bin` you don't need to run the `sqlite.db` steps above if you want to run only the `performance` benchmark.
