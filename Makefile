.DEFAULT_GOAL := help

PYTHON ?= python3
TIMEOUT ?= 600
COMPILER ?= ghc9141
TEST_SUITE ?= unit-tests
SYSTEM ?= x86_64-linux
QEMU_ARG = $(if $(QEMU),--argstr qemu "$(QEMU)",)

.PHONY: help check test test-linux-cross-wrapper test-iserv-lifecycle test-qemu-linux-user probe-rosetta-signals test-rosetta-signals verify

##@ Verification
check: ## Check whitespace and parse the cross-TH Nix expressions
	git diff --check
	nix-instantiate --parse overlays/haskell.nix >/dev/null
	nix-instantiate --parse overlays/linux-cross.nix >/dev/null
	nix-instantiate --parse overlays/armv6l-linux.nix >/dev/null
	nix-instantiate --parse overlays/qemu-linux-user.nix >/dev/null
	nix-instantiate --parse test/default.nix >/dev/null
	nix-instantiate --parse test/iserv-proxy-lifecycle.nix >/dev/null
	nix-instantiate --parse test/qemu-linux-user.nix >/dev/null
	nix-instantiate --eval --strict test/qemu-selection.nix
	nix-instantiate --parse test/rosetta-signals.nix >/dev/null

test: ## Run the existing test driver (COMPILER, TEST_SUITE, TIMEOUT)
	timeout -k 5 $(TIMEOUT) ./test/tests.sh $(COMPILER) $(TEST_SUITE)

test-linux-cross-wrapper: ## Test wrapper arguments, process ownership, and profiling selection
	$(PYTHON) test/linux-cross-wrapper.py

test-iserv-lifecycle: ## Test a built proxy on POSIX (ISERV_PROXY=/path/to/iserv-proxy)
	@test -n "$(ISERV_PROXY)" || { echo 'Set ISERV_PROXY to the built iserv-proxy executable.' >&2; exit 1; }
	$(PYTHON) test/iserv-proxy-lifecycle.py --proxy "$(ISERV_PROXY)"

verify: check test-linux-cross-wrapper test-iserv-lifecycle ## Run syntax checks and interpreter lifecycle regressions

##@ Linux emulator diagnostics
probe-rosetta-signals: ## Record host signal behavior without requiring compliant Rosetta behavior (SYSTEM)
	timeout -k 5 $(TIMEOUT) nix-build --no-out-link test/rosetta-signals.nix --argstr system $(SYSTEM)

test-rosetta-signals: ## Require correct Linux host signal behavior (SYSTEM)
	timeout -k 5 $(TIMEOUT) nix-build --no-out-link test/rosetta-signals.nix --argstr system $(SYSTEM) --arg check true

test-qemu-linux-user: ## Check guest code updates and real fault exits (SYSTEM, optional QEMU store path)
	timeout -k 5 $(TIMEOUT) nix-build --no-out-link test/qemu-linux-user.nix --argstr system $(SYSTEM) $(QEMU_ARG)

##@ Help
help: ## Show targets and their parameters
	@awk 'BEGIN { FS = ":.*##"; color = (ENVIRON["TERM"] != "" && ENVIRON["TERM"] != "dumb"); if (color) { cyan = "\033[36m"; reset = "\033[0m" } } /^##@/ { printf "\n%s\n", substr($$0, 5) } /^[a-zA-Z0-9_-]+:.*##/ { printf "  %s%-24s%s %s\n", cyan, $$1, reset, $$2 }' $(MAKEFILE_LIST)
