#
# Make the ale test modules
#

MODULES = \
	ale_SUITE


EBIN = .
ERLC = erlc

ERLDIR  := $(shell erl -noshell -eval "io:format([126,115,126,110],[code:root_dir()])" -s erlang halt)

override ERLC_FLAGS = -Wall -I ../include 

debug: ERLC_FLAGS += +debug_info -Ddebug

OBJS = $(MODULES:%=$(EBIN)/%.beam)

all: $(OBJS)

debug: all

depend:
	edep -MM -o ../ebin $(ERLC_FLAGS) $(MODULES:%=%.erl) > depend.mk

clean:
	rm -f $(OBJS) 


-include depend.mk

./%.beam:	%.erl
	erlc -o ../ebin $(ERLC_FLAGS) $<
