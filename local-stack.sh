#!/bin/sh
export STACK_YAML=stack-local.yaml
stack --work-dir=.stack-work-local $@
