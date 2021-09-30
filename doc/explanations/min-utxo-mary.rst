Min-Ada-Value Requirement
==============================

Min-Ada-Value Explanation
##########################

Recall that UTxOs on the ledger may contain a heterogeneous collection of tokens, including ada.
Ada is a limited resource in the Cardano system. Requiring some amount of ada to be included
in every UTxO (where that amount is based on the size of the UTxO, in bytes),
limits the maximum total size taken up by UTxO entries on the ledger at a given time.

The maximum possible UTxO size (the sum of the sizes of all UTxO entries) is implicitly adjusted by raising and
lowering the min-ada-value parameter. In this way, the constraint protects the Cardano ledger
from growing past a certain size. A ledger without a size bound is vulnerable to
being populated by so much data that users will unable to process it (or run a node) with
machines meeting the recommended specifications for running a node.

The Ada-only Case
###########################

Because of the min-ada-value constraint, a guaranteed
bound on the number of entries in an ada-only UTxO is

``max No. UTxOs <= (total ada in circulation) / (minimum ada value)``

This formula bounds the number of UTxO entries, but says nothing about adjusting
for the size of each entry, because all UTxO entries that contain only ada
are (close enough to) the same size. The size of the UTxO set is bounded by

``max UTxO size <= (max No. UTxOs) * (max UTxO entry size) + overhead``

In the multi-asset Cardano ledger, UTxO entries will have different sizes. In particular,
any entries containing a non-ada asset contain records of the Policy IDs, asset names,
and quantities of each of the non-ada assets. To maintain the ``max UTxO size``
bound on the UTxO stored on the ledger with variable entry sizes,
we must adjust the ``minimum ada value`` for each UTxO entry (based on its size).

Below, we present a calculation and a more detailed justification for this adjustment.


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

**Example min-ada-values and calculations**

+----------------------------------------+---------------------+
|Ada-only ``minUTxOValue`` (in lovelace) |1,000,000 (1 ada)    |
+----------------------------------------+---------------------+
| ``utxoEntrySizeWithoutVal``            |27                   |
+----------------------------------------+---------------------+
| ``coinSize``                           |0                    |
+----------------------------------------+---------------------+
| ``txoutLenNoVal``                      |14                   |
+----------------------------------------+---------------------+
| ``txinLen``                            |7                    |
+----------------------------------------+---------------------+
| ``coinsPerUTxOWord`` (in lovelace)     |37,037               |
+----------------------------------------+---------------------+

+---------------------+----------------+-----------------+------------------+------------------+------------------+
|                     | One policyID,  | One policyID,   | One PolicyID,    | One PolicyID,    | 60 PolicyIDs,    |
|                     |                |                 |                  |                  |                  |
|                     | no asset names | one 1-character | one 32-character | 110 32-character | each with one    |
|                     |                |                 |                  |                  |                  |
|                     | (i)            | asset name (ii) | asset name (iii) | names (iv)       | 32-character name|
+---------------------+----------------+-----------------+------------------+------------------+------------------+
| size of value       | 11             | 12              | 15               | 615              | 546              |
+---------------------+----------------+-----------------+------------------+------------------+------------------+
| ``minUTxO``         | 1,407,406      | 1,444,443       | 1,555,554        | 23,777,754       | 21,222,201       |
+---------------------+----------------+-----------------+------------------+------------------+------------------+
| ``minUTxO`` (in ada)| 1.407406       | 1.444443        | 1.555554         | 23.777754        | 21.222201        |
+---------------------+----------------+-----------------+------------------+------------------+------------------+

* (i) : ``6 + FLOOR (((1 * 12) + 0 + (1 * 28) + 7) / 8) = 11``

* (ii) : ``6 + FLOOR (((1 * 12) + 1 + (1 * 28) + 7) / 8) = 13``

* (iii) : ``6 + FLOOR (((1 * 12) + 32 + (1 * 28) + 7) / 8) = 15``

* (iv) : ``6 + FLOOR (((110 * 12) + 32*110 + (1 * 28) + 7) / 8) = 615``

* (v) : ``6 + FLOOR (((60 * 12) + 60*32 + (60 * 28) + 7) / 8) = 546``
