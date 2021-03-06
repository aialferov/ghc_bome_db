PROJECT = $(shell grep {application, src/*.app.src | cut -d" " -f2 | tr -d "',")
VERSION = $(shell grep {vsn, src/$(PROJECT).app.src | cut -d\" -f2)
GIT_SHA = $(shell git rev-parse HEAD | cut -c1-8)

REBAR = $(shell which ./rebar3 || which rebar3)

BASE_PATH = _build/default

LIB_PATH = $(BASE_PATH)/lib
PLUGIN_PATH = $(BASE_PATH)/plugins

USAGE_PADDING = 14

compile:
	$(REBAR) compile
	$(REBAR) unlock

check:
	$(REBAR) eunit

clean::
	find $(LIB_PATH) -type f \
		-name \*.beam -o -name \*.app -o -name erlcinfo | xargs rm -f
	find $(LIB_PATH) -type d \
		-name .rebar3 -o -name ebin | xargs rmdir 2>/dev/null || true
	find $(PLUGIN_PATH) -type f \
		-name \*.beam -o -name \*.app -o -name erlcinfo | xargs rm -f
	find $(PLUGIN_PATH) -type d \
		-name .rebar3 -o -name ebin | xargs rmdir 2>/dev/null || true

distclean:
	rm -rf _build

shell:
	$(REBAR) shell
	$(REBAR) unlock

deps:
	$(REBAR) get-deps
	$(REBAR) unlock

upgrade:
	$(REBAR) upgrade
	$(REBAR) unlock

git-release:
	git tag -a $(VERSION)
	git push origin $(VERSION)

version:
	@echo "Version $(VERSION) (git-$(GIT_SHA))"

help: usage
usage:
	$(usage)

define usage-erlanglib-targets
	@printf '$(shell printf "    %%-$(USAGE_PADDING)s %%s\\\n%.0s" {1..9})' \
	compile \
		"Compile including downloading and compiling dependencies (default)" \
	check "Run EUnit based unit tests" \
	clean "Remove all the compilation artefacts" \
	distclean "Remove the \"_build\" directory recursively" \
	shell "Run Erlang shell with all the modules compiled and loaded" \
	deps "Download dependencies" \
	upgrade "Upgrade dependencies" \
	git-release "Create and push a git tag named after current version" \
	version "Print current version and git SHA"
endef

define usage
	@echo "Usage: make <Target>"
	@echo
	@echo "Targets"
	$(usage-erlanglib-targets)
endef
