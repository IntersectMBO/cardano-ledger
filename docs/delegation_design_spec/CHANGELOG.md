# Delegation Design Document Changelog

## TBR
Incorporating the input from the workshop in Berlin, and following discussions,
into the document.

- Capture choice of KES scheme in design doc.
- Streamlining information on deposits
    
    Replacing an explanation of the concept with a link to the section where it's
    already explained.
- Elaborate on certificate replay protection
    
    The paragraph was not entirely true, it said there was only one possible source
    of funds in addition to UTxO entries, but with rewards accounts, there is
    another one.
- Fix that rewards go to treasury if reward key unregistered.
    
    We can not move them to the rewards pool -- if we did, it would create an
    incentive for all other leaders to censor a certificate that caused the pool to
    use a valid certificate.
- Update block validity to require operational key
- Additions to Operational Key section
    - They are compulsory
    - They will use KES
    - Operational Key Certificates will expire, to encourage key rotation
- Add FAQ section to the delegation design doc

## 2019-01-08
Changes after the second day of the Berlin workshop.

- Avoid overloading the term "pool"
- Clarify that Treasury is a Sink for now.
- Avoid Contention at Epoch Boundary
- Decision made: refund for stake pool paid after retirement.
- Update to non-refundable part of deposits

  We figured out in the formal spec how to incrementally add the non-refundable
  part to the reward pool of all the relevant epochs, which is fairer than
  adding all of it to the reward pool where the resource is released.
- Correction: we're introducing four address types, not three

## 2019-01-07
Changes after the first day of the Berlin workshop.

- Add todo to clarify stakepool metadata
- Add section on TTL for transactions
- Elaboration and slight change to stake pool registration.
    
  After the first day of discussions in Berlin, we came to the conclusions that
    
  - we need a _registered_ staking key to collect rewards
  - this should _not_ be the same key that's used for participating in the
    protocol. For participation in the protocol, we want cold and operational
    keys, and using the same key to withdraw rewards is detrimental.
  - We needed more elaboration on the multiple owner use case, emphasising that
    the rewards for all owners are given to the operator.
- Resolved several todo items
- Include git revision in documents
- Explain why pool registration will not be censored.

## 2018-12-18
First version that is considered stable enough to warrant V1. Some things still
need to be pinned down.
