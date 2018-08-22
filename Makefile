.PHONY: all clean deps compile test doc dialyzer xref ci

REBAR=./rebar3

DIALYZER_OPTS = # -Wunderspecs
DIALYZER_APPS = erts kernel stdlib compiler syntax_tools \
		test_server common_test

all: deps compile xref test

ci: deps compile xref dialyzer test

deps:
	$(REBAR) install_deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean
	rm -rf ./log
	rm -rf ./erl_crash.dump

clean-all: clean
	rm -rf deps

test:
	$(REBAR) ct --sys_config=config/test.config skip_deps=true verbose=3

xref:
	ERL_LIBS=./deps $(REBAR) xref skip_deps=true

doc:
	$(REBAR) doc skip_deps=true

dialyzer: deps compile
	dialyzer -r ebin $(DIALYZER_OPTS) | \
	fgrep -v -f ./dialyzer.ignore-warnings
