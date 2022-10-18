Assets, tokens, and token bundles
=============

Terminology
####################

**Asset**

An asset is an object that represents value on the blockchain. These objects can be a variety of things, such as a digital asset like ada, a role, a credential, or a quantity of goods. The term asset can refer to either:

* the identifier of a class of objects, such as 'ada' or
* a particular quantity of a specific object, such as '100 lovelace', 'this house' or 'these 10 tonnes of coffee'

An asset is uniquely identified by an asset ID.

**Asset ID**

A pair of both the **policy ID** and **asset name**. It is important to note that although ada can *act* as an asset, it is not represented using an explicit policy ID. 

Tokens that have the same asset ID have the property of being fungible with each other, and are *not* fungible with tokens that have a different asset ID. An asset ID is a unique identifier for a collection of fungible tokens.

**Policy ID**

The unique identifier that is associated with a minting policy. Let’s take a look at two policy ID examples:

``NFLPlayerCardsPolicyID`` and ``RushConcertPolicyID``.

The ID is computed by applying a hash function to the policy itself, and is thus a sequence of letters and numbers. For example:

``NFLPlayerCardsPolicyID = e0d123e5f316bef7``

**Asset name**

An (immutable) property of an asset that is used to distinguish different assets within the same policy. Unlike the *policyID*, the asset name does not refer to any code or set of rules, and can be common words. In ``TB_Example``, the names for the assets with policy ID ``RushConcertPolicyID`` are:

``Tickets`` and ``VIPTickets``

However, the policy under which an asset is scoped can specify some constraints on valid asset names. 

Different policies can use the same asset names for different tokens. For example, the token bundle:

``FAKERushConcertPolicyID { (Tickets, 500), (VIPTickets, 50) }``

contains ``Tickets`` and ``VIPTickets`` asset names, but these are not fungible with the ``RushConcertPolicyID`` tickets that have been defined in the ``TB_Example`` token bundle, as they are scoped under different policies.

**Token**

A *token* is a short term for 'asset token', which is the on-chain representation of an asset and its basic accounting unit. A token can represent one ada, one USDT, one house, or the value of ten tonnes of coffee, for example. 

**Policy or minting policy**

A set of rules that govern the minting and burning of assets scoped under that policy. There are no actual policies in ``TB_Example`` above, only references to them (ie, their IDs). The association of an asset with its minting policy is permanent, and made at the time of the asset issuance necessarily under that policy.

**Quantity**

The number of tokens with a particular asset ID that is specified in a token bundle. For example, there are 500 tokens with asset ID

``(RushConcertPolicyID, Tickets)``

in ``TB_Example``. This means that there are 500 non-vip Rush concert tickets in this bundle.

**Value**

The type which is used by the ledger to represent tokens that are tracked. Before Mary, value was just a quantity of ada. After Mary, value became a bundle of tokens with associated quantities. The ledger never talks about bare 'tokens', it always uses 'value' instead. For example:

* 1 lovelace and 3 USDT
* 1000 lovelace and 100 USDT
* Just 10 lovelace

**Script**

A piece of code that runs on the Cardano blockchain. The script is used to support smart contracts. Cardano supports several scripting languages like the multisig language in Mary and Plutus Core since Alonzo. For example, a multisig script can state that the transaction is authorized if it is signed by both Alice and Bob.

**Minting policy script**

A script that determines whether a transaction is allowed to mint or burn a particular currency. For example, a multisig script stating the minting or burning is authorized if the transaction is signed by Alice. Alice, therefore, has complete freedom to mint and burn tokens as she desires.


Token bundle
####################

A token bundle is a heterogeneous (‘mixed’) collection of tokens. Any tokens can be bundled together. Token bundles are the standard – and only – way to represent and store assets on the Cardano blockchain.

Token bundles organize tokens into a particular kind of data structure (see example and explanation below), so that which tokens are fungible with which other tokens explicitly adheres to this organization.

In previous versions of the Cardano ledger, ada amounts were specified in transaction and UTXO outputs. With the introduction of multi-asset support, these amounts have been extended with token bundles, which can specify an ada amount alongside quantities of other assets in a single output.

Token bundles are contained in outputs and mint fields of transactions, and the outputs in the UTXO set tracked by the ledger. Note that certain fields of a transaction must still explicitly specify ada amounts, such as the fee field.

Token bundle example
**********************

Here is an example of a token bundle, let’s call it ``TB_Example`` : ::

	{
		NFLPlayerCardsPolicyID {
			(SomeNFLPlayerCard, 1),
			(SomeOtherNFLPlayerCard, 1),
			(YetAnotherNFLPlayerCard, 1)}

		RushConcertPolicyID {
			(Tickets, 500),
			(VIPTickets, 50)}
	}


How and where are token bundles stored?
********************************************

Token bundles can be found: 

1. As the mint field of a transaction, indicating that the transaction is minting the tokens in the bundle.
2. In an output of a transaction or an output in the current UTXO tracked by the ledger, alongside the address of the output, e.g:

``{ MyAddress, TB_Example }``

Splitting and combining token bundles
********************************************

Transactions can arbitrarily split and combine token bundles into different bundles. Note that assets with the same ID are always fungible with each other, even when contained in separate bundles. For example, we can split the bundle ``TB_Example`` into two:

``TB_Example_Part1`` : ::

	{
		NFLPlayerCardsPolicyID {
			(SomeNFLPlayerCard, 1)}

		RushConcertPolicyID {
			(Tickets, 200),
			(VIPTickets, 20)}
	}

``TB_ExamplePart2`` : ::

	{
		NFLPlayerCardsPolicyID {
			(SomeOtherNFLPlayerCard, 1),
			(YetAnotherNFLPlayerCard, 1)}

		RushConcertPolicyID {
			(Tickets, 300),
			(VIPTickets, 30)}
	}
