REBAR = ./rebar3

BUILD_DIR = _build

all:
	$(REBAR) compile
	$(REBAR) unlock

check:
	$(REBAR) eunit

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILD_DIR)

shell:
	$(REBAR) shell
	$(REBAR) unlock
