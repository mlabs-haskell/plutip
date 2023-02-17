.PHONY: build-nix hoogle nix-build-library nix-build-executables \
	nix-build-test nix-cabal-repl requires_nix_shell ci-build-run

# Generate TOC for README.md
# It has to be manually inserted into the README.md for now.
generate-readme-contents:
	nix shell nixpkgs#nodePackages.npm --command "npx markdown-toc ./README.md --no-firsth1"

# Starts a hoogle Server.
hoogle:
	@ nix develop -c hoogle server --local --port 8008

# Attempt the CI locally
# TODO

# Build the library with nix.
nix-build-library:
	@ nix build .#plutip-core:lib:plutip

current-system := $(shell nix eval --impure --expr builtins.currentSystem)

# Build the executables with nix (also builds the test suite).
nix-build-executables:
	@ nix build .#check.${current-system}

# Build the tests with nix.
nix-build-test:
	@ nix build .#plutip-core:test:plutip-tests

# Starts a ghci repl inside the nix environment.
nix-cabal-repl:
	@ nix develop -c cabal new-repl

# Target to use as dependency to fail if not inside nix-shell.
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)

FOURMOLU_EXTENSIONS := -o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor

# Add folder locations to the list to be reformatted.
excluded := src/Test/Plutip/Internal/Cluster.hs
format:
	@ echo "> Formatting all .hs files"
	fourmolu $(FOURMOLU_EXTENSIONS) --mode inplace --check-idempotence $$(find src/ test/ local-cluster/ -iregex ".*.hs" -not -path "${excluded}")

format_check:
	@ echo "> Checking format of all .hs files"
	fourmolu $(FOURMOLU_EXTENSIONS) --mode check --check-idempotence $$(find src/ test/ local-cluster/ -iregex ".*.hs" -not -path "${excluded}" )

NIX_SOURCES := $(shell fd -enix)

nixpkgsfmt: requires_nix_shell
	nixpkgs-fmt $(NIX_SOURCES)

nixpkgsfmt_check: requires_nix_shell
	nixpkgs-fmt --check $(NIX_SOURCES)

CABAL_SOURCES := $(shell fd -ecabal)

cabalfmt: requires_nix_shell
	cabal-fmt --inplace $(CABAL_SOURCES)

cabalfmt_check: requires_nix_shell
	cabal-fmt --check $(CABAL_SOURCES)

lint: requires_nix_shell
	hlint $$(find src/  -iregex ".*.hs"  -not -path "${excluded}") $$(find test/ -iregex ".*.hs")
