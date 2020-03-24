# Address Discrimination (`NetworkMagic`)

Address discrimination was a problem in the original
[`cardano-sl`](https://github.com/input-output-hk/cardano-sl) implementation
that came to our attention in mid-2018. This document briefly describes the
problem as well as the solution implemented.

For those with access to IOHK's old internal issue tracker, YouTrack, all of
the nitty-gritty details can still be found under issues `CO-353` and
`CO-354`.

## The Problem

In the original `cardano-sl` design, there was no way of distinguishing
between addresses of different networks. This was undesirable because, for
example, it was totally valid for a user to attempt a transfer of funds from a
mainnet/staging address to a testnet address and vice versa which could result
in a loss of those funds.

## The Solution

Our solution was to introduce a new field to addresses which would contain a
specified protocol magic value (a magic value used to distinguish between
networks). That way, we could check the protocol magic values contained within
each address when validating transactions.

However, because pre-existing mainnet and staging addresses would not contain
this new field, we had to ensure that its inclusion would be optional in order
to maintain backward compatibility.

### `NetworkMagic`

Given these requirements, we introduced a new data type to be used in
representing this new field in each address:

```
data NetworkMagic
  = NetworkMainOrStage
  | NetworkTestnet !Word32
```

`NetworkMagic` effectively represents an _optional_ protocol magic ID value.
Depending on the type of network, `NetworkMagic` is utilized in different ways
with regard to its inclusion in addresses:

  - For maintaining backward compatibility, all mainnet and staging addresses
  *must not* contain a network magic, i.e. they would contain a value of
  `NetworkMainOrStage`.

  - All testnet addresses *must* contain a network magic, i.e. they would
  contain a value of `NetworkTestnet`. At the time, we decided that it wasn't
  necessary to maintain backward compatibility with pre-existing testnet
  addresses.

### `RequiresNetworkMagic`

We also required a way to specify, via the node's configuration, whether a
network requires the inclusion of network magic in its addresses. Thus, we
introduced a new data type, `RequiresNetworkMagic`:

```
data RequiresNetworkMagic
  = RequiresNoMagic
  | RequiresMagic
```

We also added a `RequiresNetworkMagic` field to the `ProtocolMagic` data type.
The `ProtocolMagic` data type would now contain two fields for which values
would be specified in the configuration:

  - One field for `RequiresNetworkMagic` which is specified in the node's
  configuration.

  - One field for the actual protocol magic ID (essentially a `Word32`) which
  is specified in the genesis configuration.

Here's a simplified version of the `ProtocolMagic` data type to display the
result of adding this new field:

```
data ProtocolMagic = ProtocolMagic !Word32 !RequiresNetworkMagic
```

One might question why we didn't just allow the `NetworkMagic` value to be
specified in the node's configuration as opposed to adding this new
`RequiresNetworkMagic` type. After all, this would clearly indicate whether
the network requires a `NetworkMagic` value to be serialized in its addresses
since there are only two clear cases:

  - `NetworkMainOrStage` - No network magic value serialized in addresses.

  - `NetworkTestnet !Word32` - Network magic value serialized in addresses.

However, the problem with this is that, in the `NetworkTestnet` case, the
configuration could potentially specify a `Word32` value that differs from
the protocol magic ID value already specified in the immutable genesis
configuration. Because these values _must_ be the same, we figured it'd make
sense to limit the potential for human error and, instead, derive the
`NetworkMagic` value from the `ProtocolMagic` whenever necessary.

Here's a simplified version of how this derivation might look:

```
makeNetworkMagic :: ProtocolMagic -> NetworkMagic
makeNetworkMagic (ProtocolMagic pmId reqNetMagic) = case reqNetMagic of
  RequiresNoMagic -> NetworkMainOrStage
  RequiresMagic   -> NetworkTestnet pmId
```

### Transaction Validation With Address Discrimination

With the "address discrimination" functionality:

  - You cannot transfer funds from a mainnet/staging address to a testnet
  address as the testnet address will include a `NetworkMagic` while the
  mainnet/staging node's configuration specifies that it `RequiresNoMagic`.

  - You cannot transfer funds from a testnet address to a mainnet/staging
  address as the mainnet/staging address does not include a `NetworkMagic`
  while the testnet node's configuration specifies that it `RequiresMagic`.

  - You cannot transfer funds between addresses that were generated on
  different testnets as their `NetworkMagic` values will not all be equal.
