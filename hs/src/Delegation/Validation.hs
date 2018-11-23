module Delegation.Validation
  ( ValidationError(..)
  ) where

-- Specific validation errors for transactions.
data ValidationError =
                     -- | The UTxO inputs in the transation are not valid.
                       BadInputs
                     -- | The transaction results in an increased total balance of the ledger.
                     | IncreasedTotalBalance
                     -- | The transaction does not have all the transaction witnesses required.
                     | InsufficientTxWitnesses
                     -- | The transaction does not have all the certificate witnesses required.
                     | InsufficientCertWitnesses
                     -- | The transaction contains an invalid stake registration certificate.
                     | BadRegistration
                     -- | The transaction contains an invalid stake deregistration certificate.
                     | BadDeregistration
                     -- | The transaction contains an invalid stake delegation certificate.
                     | BadDelegation
                     -- | The transaction contains an invalid pool registration certificate.
                     | BadPoolRegistration
                     -- | The transaction contains an invalid pool retirement certificate.
                     | BadPoolRetirement
                     deriving (Show, Eq)
