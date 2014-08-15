#
# Makefile for the RVI node.
# 
#

.PHONY:	all deps compile setup clean doc setup_backend setup_device


NAME=rvi
SETUP_GEN=./deps/setup/setup_gen



all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar  compile

recomp:
	./rebar  compile skip_deps=true


# target_backend:
# 	ERL_LIBS=$(PWD)/deps:$(ERL_LIBS) \
# 	$(SETUP_GEN) $(NAME) priv/backend.config setup -pz $(PWD)/ebin \
# 	-target rel_backend -vsn 0.1

# target_device:
# 	ERL_LIBS=$(PWD)/deps:$(ERL_LIBS) \
# 	$(SETUP_GEN) $(NAME) priv/setup_device.config setup -pz $(PWD)/ebin \
# 	-target rel_device -vsn 0.1

# doc:
#         ./REBAR_DOC=1 ./rebar skip_deps=true get-deps doc

clean:
	./rebar clean
