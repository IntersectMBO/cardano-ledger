#!/usr/bin/env bash

set -euo pipefail

# Author: michal.rus@iohk.io

export NETWORK=mainnet
export UNPACK_DIR=cardano-db
export MITHRIL_VERSION=2513.0

usage()
{
	cat <<-EOF
		Usage: $(basename "$0") [-h] [-d DIR] [-v VERSION] NETWORK
		  Download cardano-db using mithril

		Options:
		  -h          Show this help text
		  -d DIR      Unpack the DB into DIR (default: $UNPACK_DIR)
		  -v VERSION  Use version VERSION of the mithril client (default: $MITHRIL_VERSION)

		Arguments:
		  NETWORK     Download the DB for NETWORK (default: $NETWORK)
	EOF

	exit "${1:-0}"
}

while getopts hd:v: OPT
do
	case "$OPT" in
		\?) usage 1 >&2;;
		h)  usage;;
		d)  UNPACK_DIR=$OPTARG;;
		v)  MITHRIL_VERSION=$OPTARG;;
	esac
done

shift $((OPTIND - 1))

case $# in
	0) ;;
	1) NETWORK=$1;;
	*) usage 2 >&2;;
esac

case "$NETWORK" in
	"mainnet") MITHRIL_NETWORK="release-mainnet" ;;
	"preprod") MITHRIL_NETWORK="release-preprod" ;;
	"preview") MITHRIL_NETWORK="pre-release-preview" ;;
	*) echo >&2 "fatal: invalid NETWORK value: $NETWORK"; exit 1 ;;
esac
export MITHRIL_NETWORK

export AGGREGATOR_ENDPOINT="https://aggregator.${MITHRIL_NETWORK}.api.mithril.network/aggregator"

# shellcheck disable=SC2016
nix shell \
		"github:input-output-hk/mithril/${MITHRIL_VERSION}#mithril-client-cli" \
		nixpkgs#jq \
		nixpkgs#bash \
		nixpkgs#curl \
		--command bash -c '
	set -euo pipefail

	SNAPSHOT_DIGEST=$(mithril-client cardano-db snapshot list --json | jq -r ".[0].digest")

	GENESIS_VERIFICATION_KEY=$(curl -fsSL "https://raw.githubusercontent.com/input-output-hk/mithril/${MITHRIL_VERSION}/mithril-infra/configuration/${MITHRIL_NETWORK}/genesis.vkey")
	export GENESIS_VERIFICATION_KEY

	set -x
	mithril-client cardano-db download "$SNAPSHOT_DIGEST" --download-dir "$UNPACK_DIR"
'
