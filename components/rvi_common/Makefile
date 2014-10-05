.PHONY:	all deps compile clean recomp


all: deps compile

deps:
	rebar get-deps

compile:
	rebar compile

recomp:
	rebar compile skip_deps=true

clean:
	rebar clean




