# Repository structure

This repo contains formal (LaTeX) and executable (Haskell model) specs for both
the Byron and Shelley eras of Cardano. The outline of the specs is as follows:

- [byron](./byron)
  - [ledger](./byron/ledger)
    - [formal-spec](./byron/ledger/formal-spec)
    - [executable-spec](./byron/ledger/executable-spec)
  - [chain](./byron/chain)
    - [formal-spec](./byron/chain/formal-spec)
    - [executable-spec](./byron/chain/executable-spec)
- [shelley](./shelley)
  - [design-spec](./shelley/design-spec)
  - [chain-and-ledger](./shelley/chain-and-ledger) (specs are combined in Shelley era)
    - [formal-spec](./shelley/chain-and-ledger/formal-spec)
    - [executable-spec](./shelley/chain-and-ledger/executable-spec)
    - [dependencies](./shelley/chain-and-ledger/dependencies)