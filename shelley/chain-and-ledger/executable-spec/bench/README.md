# Benchmarking

## Running with Stack

The benchmark suite can be run from the command line with:

```shell
stack bench
```

The benchmark groups can be specified by name, eg:

```shell
stack bench --ba 'utxo'
stack bench --ba 'stake-key/register'
```

More output is available by specifying at html output file:

```shell
stack bench --ba --output=bench.html
```
