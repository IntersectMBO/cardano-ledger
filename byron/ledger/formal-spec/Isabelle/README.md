Requirements
============

You need Isabelle2018 to use the Isabelle developments in this
directory. You can obtain Isabelle2018 from the [Isabelle
website][isabelle].

[isabelle]:
    http://isabelle.in.tum.de/
    "Isabelle"


Setup
=====

To make the Isabelle developments in this directory available to your
Isabelle installation, please add the path of this directory to the file
`$ISABELLE_HOME_USER/ROOTS`. You can find out the value of
`$ISABELLE_HOME_USER` by running the following command:

    isabelle getenv ISABELLE_HOME_USER


Building
========

Running `make` builds the PDF documents for the different Isabelle
libraries and places them in `$ISABELLE_BROWSER_INFO/Ledger_Rules`. You can
find out the value of `$ISABELLE_BROWSER_INFO` by running the following
command:

    isabelle getenv ISABELLE_BROWSER_INFO

The makefile specifies two targets: `properly`, which is the default,
and `qnd`. With `properly`, fake proofs (`sorry`) are not accepted; with
`qnd`, quick-and-dirty mode is used and thus fake proofs are accepted.

