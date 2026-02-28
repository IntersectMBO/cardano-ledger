# cabal-logs #

This project contains utilities for examining and summarizing Cabal log files.

## extract-failures ##

`extract-failures` reads the Cabal build plan file and uses it to learn the
target names and log file locations of all tests within the Cabal project. It
then parses any logs that exist and extracts the information needed to
reproduce any failures (seed and pattern). It saves a copy of this information
as JSON.

`extract-failures` understands the output of both Hspec- and Tasty-based tests.

## render-failures ##

`render-failures` reads a set of JSON files created by `extract-failures`,
merges them, and outputs a summary in Markdown format.

This output is suitable for including in a GitHub job summary, so users can
quickly see what failed in a CI run without having to open the log files
themselves.
