# Concrete implementation decisions

This file is a repository to capture concrete decisions that have to be made in
order to implement the specs found in this repo. The idea here is that we have a
record of extra decisions which are outside of the scope of the formal spec.

When such a case is found a section should be added to this document outlining
the decision made and the section of the spec or rule that the decision
corresponds to. In a specific format:

## Format

The format is similar to the Architectural Decision Record [^1] used in issues
in the wallet. This consists of:

### Context


<!-- WHEN PROPOSED
A few sentences outlining the decision to be made and the section of the spec
the decision relates to.
Give any elements that help understanding where this issue comes from. Leave no
room for suggestions or implicit deduction.
-->


#### Decision

<!-- WHEN PROPOSED
Give details about the architectural decision and what it is doing. Be
extensive: use schemas and references when possible.
-->

### PR

<!-- WHEN IN PROGRESS
List of all PRs related to this ticket including the PR proposing the decision.

e.g.

| Number                                       | Base            |
| ---                                          | ---             |
| https://github.com/some/other-repo/issues/14 | `develop`       |
| #42                                          | `release/2.0.0` |
-->


### Development Plan

<!-- WHEN IN PROGRESS If the proposal is non trivial add details of intended
implementation in the form of a TODO list, explain how the ticket is going to be
tackled and how you intend to proceed.

e.g.

- [ ] I intend to extend the existing handlers and use the wallet layer to implement
  the necessary steps.

- [ ] I plan on testing the endpoint by adding a few integration scenarios
-->

### Implications

<!-- WHEN CLOSED Any ongoing implications of this decision might have in future.
In some cases the decision made will warrant further work later or has
ramifications for the future that should be recorded here. e.g.

- Turns out the wallet layer wasn't implemented at all so this has to be done as an extra step.
- Integration tests are running but now takes an unexpected long time. I'll open a ticket to investigate
  this regression.
-->


## Process

This file should be edited by adding a section matching the format above in a PR
against this repo. The PR should reference and issues in the implementation repo.


After that there are a few options:


### The decision is agreed to

The proposal becomes the choice for concrete implementations henceforth. The PR
is approved and merged


### The decision needs refinement

This refinement is discused in the PR proposing the decision, the section is
ammended until we agree on the decision (this should be timeboxed) as we can
always revisit any decisions.

[^1]: https://adr.github.io/

## Decisions

This is the section that should be appended to when decisions are made

### This is where the title goes

#### Context

#### Decision

#### PR

| Number                                       | Base            |
| ---                                          | ---             |
| https://github.com/some/other-repo/issues/14 | `develop`       |
| #42                                          | `release/2.0.0` |


#### Development Plan

#### Implications
