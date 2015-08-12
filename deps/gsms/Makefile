.PHONY: all compile deps clean shell

all: compile

deps:
	rebar get-deps

compile: deps
	rebar compile

clean:
	rebar clean

shell: compile
	ERL_LIBS=$(PWD)/deps erl -pa ebin
