
.PHONY: all compile clean eunit test doc

all: compile

compile:
	rebar compile

eunit: compile
	rebar eunit

test: eunit

clean:
	rebar clean

doc:
	rebar doc