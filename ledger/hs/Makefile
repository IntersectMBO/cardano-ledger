help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid: ## Run ghcid
	ghcid \
					--command "cabal new-repl ledger-simple"

ghcid-test: ## Have ghcid run the test suite
	ghcid \
          --command "stack ghci utxo:lib utxo:test:utxo-test --ghci-options=-fobject-code" \
          --test "main"

.PHONY: ghcid ghcid-test help
