.PHONY:	all deps compile setup clean doc


NAME=service_edge
export KVDB_BACKENDS=ets

SETUP_GEN=$(shell ./find_setup_gen.sh)

all: deps compile

deps:
	rebar get-deps

compile:
	rebar  compile

recomp:
	rebar  compile skip_deps=true

setup:
	ERL_LIBS=$(PWD)/deps:$(ERL_LIBS):$(PWD) \
	$(SETUP_GEN) $(NAME) priv/setup.config setup

target:
	ERL_LIBS=$(PWD)/deps:$(ERL_LIBS) \
	$(SETUP_GEN) $(NAME) priv/setup.config setup -pz $(PWD)/ebin \
	-target rel -vsn 0.1

run: setup
	erl -boot setup/start -config setup/sys

doc:
        REBAR_DOC=1 rebar skip_deps=true get-deps doc

clean:
	rebar clean




