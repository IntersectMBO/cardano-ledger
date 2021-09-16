Min-Ada-Value Calculation in Alonzo
===================================

The calculation
#################

There is no longer a ``minUTxOValue`` protocol parameter in Alonzo, it was
deprecated at the end of the Mary era. However, the requirement that each
UTxO must contain an amount of ada that depends on the size of the entry is
still enforced in the Alonzo era.
The size-dependent minimum ada amount in a UTxO is still referred to as the min-ada-value, and
is now calculated using the Alonzo parameter ``coinsPerUTxOWord``.

The formula for the min-ada-value calculation, for a UTxO containing an output ``txout`, is:

``utxoEntrySize (txout) * coinsPerUTxOWord``

where

``utxoEntrySize (txout) = utxoEntrySizeWithoutVal + size (v) + dataHashSize (dh)``

The ``v`` variable is the token bundle inside the ``txout`` output.
The minimum ada value calculation relies on the ``size`` function for determining
the size of a token bundle or a lovelace value, which is described in
the Mary era min-value document.

The variable ``dh`` is the hash of the datum that is contained in the output. If there is
no datum, the hash is represented by ``Nothing``, and takes up ``0`` space. If there
is a datum in the output, it takes up ``10`` words of memory space. 

Example min-ada-value calculations and current constants
#########################################################

Note that the ``coinsPerUTxOWord`` is a protocol parameter and is subject to
change. The values ``utxoEntrySizeWithoutVal`` and ``dataHashSize (dh)``
are fixed at least for the entire Alonzo era.

The following table gives the values of the constants used in the calculation above.
Recall that

``1 ada = 1,000,000 lovelace``

+------------------------------------------+---------------------+
| Ada-only min-utxo value                  |1,000,000 lovelace   |
+------------------------------------------+---------------------+
| ``utxoEntrySizeWithoutVal``              |27 words             |
+------------------------------------------+---------------------+
| ``coinsPerUTxOWord``                     |34,482 lovelace      |
+------------------------------------------+---------------------+
| ``dataHashSize (dh)``, ``dh = Nothing``  |0 words              |
+------------------------------------------+---------------------+
| ``dataHashSize (dh)``, ``dh <> Nothing`` |10 words             |
+------------------------------------------+---------------------+

** NO datum hash: **

+--------------------------+-----------------+-----------------+-------------------+------------------+------------------+---------------------------------+
|                          | One policyID,   | One policyID,   | One PolicyID,     | Two PolicyIDs,   | Two PolicyIDs,   | Three PolicyIDs,                |
|                          |                 |                 |                   |                  |                  |                                 |
|                          | one 0-character | one 1-character | three 1-character | one 0-character  | one 1-character  | ninety-six 1-character          |
|                          |                 |                 |                   |                  |                  |                                 |
|                          | asset name (i)  | asset name (ii) | asset names (iii) | name (iv)        | name for each (v)| names between them (total) (vi) |
+--------------------------+-----------------+-----------------+-------------------+------------------+------------------+---------------------------------+
| size of value            | 11              | 12              | 15                | 16               | 17               | 173                             |
+--------------------------+-----------------+-----------------+-------------------+------------------+------------------+---------------------------------+
| ``utxoEntrySize``        | 38              | 39              | 42                | 43               | 44               | 20                              |
+--------------------------+-----------------+-----------------+-------------------+------------------+------------------+---------------------------------+
| ``minUTxO`` (in lovelace)| 1,310,316       | 1,344,798       | 1,448,244         | 1,482,726        | 1,517,208        | 6,896,400                       |
+--------------------------+-----------------+-----------------+-------------------+------------------+------------------+---------------------------------+

** WITH datum hash: **

+--------------------------+-----------------+--------------------+------------------+
|                          | One policyID,   | One PolicyID,      | Two PolicyIDs,   |
|                          |                 |                    |                  |
|                          | one 0-character | three 32-character | one 0-character  |
|                          |                 |                    |                  |
|                          | asset name (i)  |  asset name (vii)  | name (viii)      |
+--------------------------+-----------------+--------------------+------------------+
| size of value            | 11              | 26                 | 16               |
+--------------------------+-----------------+--------------------+------------------+
| ``utxoEntrySize``        | 48              | 63                 | 43               |
+--------------------------+-----------------+--------------------+------------------+
| ``minUTxO`` (in lovelace)| 1,655,136       |  2,172,366         | 1,827,546        |
+--------------------------+-----------------+--------------------+------------------+

The following are calculations of the sizes of token bundles (values)
described in the tables above. These are according to the ``size`` function
give in the min-ada-value document for the Mary era.

* (i) : ``6 + FLOOR(((1 * 12) + 0 + (1 * 28) + 7)/8, 1) = 11``

* (ii) : ``6 + FLOOR(((1 * 12) + 1 + (1 * 28) + 7)/8, 1) = 12``

* (iii) : ``6 + FLOOR(((3 * 12) + (3*1) + (1 * 28) + 7)/8, 1) = 15``

* (iv) : ``6 + FLOOR(((2 * 12) + 0 + (2 * 28) + 7)/8, 1) = 16``

* (v) : ``6 + FLOOR(((2 * 12) + (1*2) + (2 * 28) + 7)/8, 1) = 17``

* (vi) : ``6 + FLOOR(((96 * 12) + 96 + (3 * 28) + 7)/8, 1) = 173``

* (vii) : ``6 + FLOOR(((3 * 12) + 96 + (1 * 28) + 7)/8, 1) = 26``

* (viii) : ``6 + FLOOR(((3 * 12) + 96 + (1 * 28) + 7)/8, 1) = 16``


Constraint on Token Bundle Size
##################################

In addition to the min-ada-value requirement, the ledger enforces a constraint
on the sizes of token bundles in transaction outputs. The reason for having this
constraint is to ensure that any single output is, in most cases, not so big
that a transaction attempting to spend it would have to exceed the maximum
transaction size.

For this constraint, the size of the token bundle is not estimated (as for the min-ada-value
constraint), but rather taken to be the size of the serialized representation of
the token bundle. We do not provide a formula for the serialized size calculation here.
The following protocol parameter gives this constraint, and has the
following current value currently (subject to change):

``maxValSize = 4000`` bytes, ie. ``500`` words
