PROJECT=el_math

ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif

.PHONY: all test clean

all: release

deps:
	@$(REBAR3) get-deps

compile:
	@$(REBAR3) as local compile

release:
	@$(REBAR3) as local release -n ${PROJECT}

clean:
	@$(REBAR3) clean

cleanall:
	rm -rf ./_build && rm -rf ./rebar.lock

production_release:
	@$(REBAR3) as production release -n $(PROJECT)

run:
	$(REBAR3) as production shell 

tar:
	$(REBAR3) as production tar

debug:
	ERL_FLAGS="-args_file ./config/local/vm.args.debug" $(REBAR3) as local shell

