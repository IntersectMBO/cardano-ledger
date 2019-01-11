## Decisions

This is the section that should be appended to when decisions are made

### Cryptographic Primitives (Figure 2)

#### Context

Cryptograhic primitives: signing keys. Is it going to be the same curve
as for the transaction witnesses?

#### Decision

I belive to be consistent we can continue to use ed25519 signature s

#### PR

| Number                                       | Base            |
| ---                                          | ---             |
| https://github.com/input-output-hk/fm-ledger-rules/issues/SELF | `develop`       |
| N/A                                          | N/A             |


#### Development Plan

N/A

#### Implications

very little needs to be done here, it is only pinning down the decision
