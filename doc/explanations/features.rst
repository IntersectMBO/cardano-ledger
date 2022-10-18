How does a native token compare to ada and ERC20?
==================================================================

Native tokens behave the same as ada in most cases. However, ada is Cardano's *principal* currency and is the only currency used for such special purposes as paying fees or earning rewards.

+-----------------------------------+-------+-----------------+-------------------------------------------------+
|                                   | Ada   | Native tokens   | Comment                                         |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be sent in transactions        | Y     | Y               |                                                 |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be kept in UTXO outputs        | Y     | Y               |                                                 |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be locked with script outputs  | Y     | Y               |                                                 |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be sent to an exchange address | Y     | Y               |                                                 |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be minted/burned               | N     | Y               | Ada cannot be created or destroyed,             |
|                                   |       |                 |                                                 |
|                                   |       |                 | its policy ID does not correspond to a script.  |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be used to pay fees, receive   | Y     | N               | Ada is the only currency which can be used for  |
|                                   |       |                 |                                                 |
|rewards, etc.                      |       |                 | paying fees and earning rewards.                |
+-----------------------------------+-------+-----------------+-------------------------------------------------+
|Can be used to cover the minimum   | Y     | N               | Ada is the only currency which can be used      |
|                                   |       |                 |                                                 |
|UTXO value                         |       |                 | for deposits.                                   |
+-----------------------------------+-------+-----------------+-------------------------------------------------+

How do native tokens compare to ERC20 tokens?
###############################################

ERC20 is an Ethereum token standard, widely used for the purpose of token issuance on various platforms. The peculiarity of this token type lies in the fact that it can represent value and serve for such purposes as payments, value transfer, exchange, rewards or incentives, access to services and products, represent voting rights, etc. Also, these tokens can hold both utility and security features, which opens a range of possible use cases for businesses, applications, and enterprises. 

On Cardano, users can create native tokens that will serve the above-mentioned purposes and in addition, it is possible to create *unique* (non-fungible) assets representing value like real estate or intellectual rights, for example (in Ethereum, this functionality requires a separate standard, ERC721). 

Unlike ERC20 tokens, the tracking and accounting of native tokens is supported by the ledger natively (ERC20 tokens require smart contracts to achieve the same thing). An important benefit of using native tokens is that they do not require smart contracts to transfer their value and can be transferred alongside other token types. Also, unlike ERC20, native tokens do not require special transfer fees or additional event-handling logic to track transactions. 

+------------------------------------+-----------------------------------------+--------------------------------------------+
|                                    | ERC20                                   |Native tokens                               |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Blockchain                          | Ethereum                                |Cardano                                     |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Relationship to the blockchain      | A contract standard, users copy-paste   |Not a standard. Most functionality          |
|                                    |                                         |                                            |
|                                    | the standard code and modify it.        |is built into the ledger itself.            |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Controlled by                       | A Solidity smart contract               |A minting policy script in any scripting    |
|                                    |                                         |                                            |
|                                    |                                         |language supported by Cardano               |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Requires a smart contract           | Y                                       |Y                                           |
|                                    |                                         |                                            |
|to mint/burn?                       |                                         |                                            |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Minting logic can be customized?    | Y                                       |Y                                           |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Requires a smart contract           | Y                                       |N                                           |
|                                    |                                         |                                            |
|to transfer?                        |                                         |                                            |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Can be used by other smart          |                                         |                                            |
|                                    |                                         |                                            |
|contracts without special support?  | N                                       |Y                                           |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Can be transferred alongside other  | N                                       |Y                                           |
|                                    |                                         |                                            |
|tokens?                             |                                         |                                            |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Transfer logic provided by          | Copy-pasting from the ERC20 template    |The Cardano ledger itself                   |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Transfer logic can be customized    | Y                                       |N                                           |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Requires special fees to transfer   | Y                                       |N                                           |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Requires additional event-handling  | Y                                       |N                                           |
|                                    |                                         |                                            |
|logic to track transfers            |                                         |                                            |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Supports non-fungible tokens        | N                                       |Y                                           |
+------------------------------------+-----------------------------------------+--------------------------------------------+
|Readable metadata                   | Provided by the operating smart         |Provided by the off-chain metadata server   |
|                                    |                                         |                                            |
|                                    | contract                                |                                            |
+------------------------------------+-----------------------------------------+--------------------------------------------+


Security
####################

ERC20 tokens have proven vulnerable to a wide range of `security issues <https://peckshield.medium.com/alert-new-batchoverflow-bug-in-multiple-erc20-smart-contracts-cve-2018-10299-511067db6536>`_. This is conditioned by the fact that ERC20 token creation requires manual modification of the contract standard, which can result in errors and possible bugs. Creating and transacting tokens natively removes the possibility of human error, since the ledger itself handles the token logic. Additionally, over- and under-flow vulnerabilities that are present for ERC20 are eliminated for native tokens, as Cardano’s scripting language does not have fixed-size integers and the ledger itself (rather than the ERC20 user code) tracks tokens movement.

+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
|                                        |ERC20    |Native tokens  |Comment                                                               |
+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
|User errors in copying standard code    |Y        |N              |All shared functionality is provided by the ledger                    |
+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
|Over-/under-flow vulnerabilities        |Y        |N              |Cardano’s scripting languages don’t have fixed-size integers          |
+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
|Unprotected functions                   |Y        |N              |User code is called only in very specific cases  to validate minting. |
+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
|Denial of service via gas price attacks |Y        |N              |Denial of service attacks on the entire system are still possible.    |
+----------------------------------------+---------+---------------+----------------------------------------------------------------------+
