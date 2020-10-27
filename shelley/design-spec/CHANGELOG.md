# Delegation Design Document Changelog

## 2020-10-08
Include member stake in the non-myopic stake calculation.
Replaced average apparent performance usage with references to the stake pool ranking document.

## 2020-06-12
Rewrote the chapter on addresses. Now includes multi-sig, and is clearly separating addresses (payment and stake) and credentials.

## 2020-02-28
Clarify when we use active stake vs total stake.

## 2019-06-07
Update section on script addresses.

## 2019-05-17
Some clarifications in response to review by the auditors.

## 2019-04-11
Some subtle corrections in the rewards chapter after review by Aikaterina.

First version officially published on the IOHK blog.

## 2019-04-08
General review of the document.

Mostly small things. Consistent wording, spelling, readability, removed some
obsolete things.

Removed the remaining todo items. Decisions we still need to make are now
tracked on github instead.

Moved the section on detecting stale stake into an appendix, and changed it to
reflect the design where stake that is not delegated to an active pool is
ignored (which solves the problem of stale stake to a large degree).

## 2019-04-05
Rewrote the chapter on rewards.

We had lots of discussions about how to properly account for the performance of
pools, particularly in Praos where the actual performance is not observable due
to the private leader schedule. We finally converged to a solution, leading to a
re-write of the incentives section.

Also, changed the title of the document, and made the capitalisation of ada
consistent.

## 2019-03-01
Incorporating further input from the workshop in Berlin, and following discussions,
into the document.

- Decision: transactions have to have at least one UTxO style input

- Update: Stake pool metadata

  Specify the format for the metadata, and how it is provided. Streamlined the
  different sections that touch stake pool metadata.
- Elaborating further on why stake pool registrations will not be censored.
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
    - Slight Change in validity rules
- Add FAQ section to the delegation design doc
- Add a couple of todo entries


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
