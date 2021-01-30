Min-Ada-Value Requirement
==============================

Min-Ada-Value Explanation
##########################

Recall that outputs may contain a heterogeneous collection of tokens, including ad Ada is a limited resource in the Cardano system. Requiring some amount of ada be included in every output on the ledger (where that amount is based on the size of the output, in bytes) protects the size of the Cardano ledger from growing intractably.

Min-ada-value Calculation
###########################

The minimum ada amount required to be contained in every ada-only UTxO with no additional data (i.e. a UTxO containing only the address and ada amount) is a protocol parameter of the Cardano system, called : ``minUTxOValue``

The size of such a UTxO has a upper bound : ``adaOnlyUTxOSize``

We can calculate the upper bound on size of a UTxO u containing non-ada tokens : ``sizeBound (u)``

We want to calculate the min-ada required to be contained in u : ``minAda (u)``

A minUTxOValue amount of ada pays for ``adaOnlyUTxOSize`` bytes of UTxO storage on the ledger. To make the min-ada-value proportional for all UTxOs, the following proportion must be satisfied :

	``minUTxOValue / adaOnlyUTxOSize = minAda (u) / sizeBound (u)``

The min-ada calculation for any UTxO approximates the above formula. This uses the constants,

  ``coinSize = 0`` (note: this is an implementation error, and will be changed to the correct value, 2, in the next fork. This will decrease the minimum ada value by a small percentage)

  ``utxoEntrySizeWithoutVal = 27``

  ``adaOnlyUTxOSize = utxoEntrySizeWithoutVal + coinSize = 27``

The functions used in the formula below are :

* ``quot a b`` is the quotient of ``(a, b)``, ie.
  ``quot 11 5 = 2`` because ``11 = 5 * 2 + 1``

* ``numAssets`` : the number of distinct AssetIDs in ``B``, eg. if
  ``B = [(policyID1, myAssetNameSBS, 10),
  (policyID1, yourAssetNameSBS, 5),
  (policyID2, someAssetNameSBS, 250),
  (policyID3, someAssetNameSBS, 43)]``

  ``numAssets B = 4``

* ``numPIDs`` : the number of distinct PolicyIDs in ``B``
  eg. if ``B`` is as above, ``numPIDs B = 3``
  (which are ``policyID1, policyID2, policyID3``)

* ``sumAssetNameLengths`` : the sum of the length of the ByteStrings representing distinct asset names
  eg. if ``B`` is as above,

  ``sumAssetNameLengths B = length myAssetNameSBS + length yourAssetNameSBS + length someAssetNameSBS``

* ``pidSize`` : the length of the hash of a policy (ie. the length of the PolicyID). These lengths are the same for all policies, and are dictated by the current hashing algorithm used to compute payment and staking address hashes.

  ``pidSize B = 28`` currently (also, in the next era)

* ``roundupBytesToWords`` converts bytes to 8-byte long words, rounding up
  ``roundupBytesToWords b = quot (b + 7) 8``

* ``size B`` is size of the token bundle ``B`` in 8-byte long words :
  ``size B = 6 + roundupBytesToWords (((numAssets B) * 12) + (sumAssetNameLengths B) + ((numPids B) * pidSize))``

For a UTxO containing a token bundle ``B`` the min-ada-value calculation is as follows :

* Case 1 : Token bundle ``B`` in the UTxO ``u`` contains only ada (no other tokens)
  ``minAda (u) = minUTxOValue``

* Case 2 : Token bundle ``B`` in the UTxO ``u`` contains ada as well as other tokens
  ``minAda (u) = max (minUTxOValue, (quot (minUTxOValue, adaOnlyUTxOSize)) * (utxoEntrySizeWithoutVal + (size B)))``


**As a consequence of this design,**

* It is impossible to make outputs containing only custom tokens
* The number of each kind of token in an output does not affect the min-ada-value of the output, but the number of types of tokens contained in an output increases the min-value.
* The reason for this is that the names and policy IDs of each of the types of tokens take up additional space in the output.
* Sending custom tokens to an address always involves sending the min-ada-value of ada to that address alongside the custom tokens (by including the ada in the same output). If the address is not spendable by the user sending the tokens, the ada sent alongside the tokens no longer belongs to the sender.
* Before transferring custom tokens, users may choose to use off-chain communication to negotiate who supplies the ada to cover the min-ada-value in the output made by the transferring transaction
* To recover the ada stored alongside custom tokens in an output O, the user must either:
  a) Spend the output O, and burn the custom tokens stored therein
  b) Spend an output O and an output O’, and consolidate the tokens therein with the same collection of types of custom tokens stored in another output (spent within the same transaction)

Eg. ``(CryptoDoggiesPolicy, poodle, 1)`` contained in O can be consolidated with
``(CryptoDoggiesPolicy, poodle, 3)`` in O’, for a total of ``(CryptoDoggiesPolicy, poodle, 4)`` in a new output made by the consolidating transaction.

* Splitting custom tokens into more outputs than they were contained in before the transaction getting processed requires using, in total, more ada to cover the min-ada-value, as ada is needed in the additional outputs.
