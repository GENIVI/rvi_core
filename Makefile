.PHONY:	all deps compile setup clean doc setup_backend setup_device


NAME=rvi
export KVDB_BACKENDS=ets

SETUP_GEN=$(shell ./find_setup_gen.sh)


all: deps compile

deps:
	rebar get-deps

compile:
	rebar  compile

recomp:
	rebar  compile skip_deps=true

setup_device:
	ERL_LIBS=$(PWD)/deps:$(ERL_LIBS):$(PWD) \
	$(SETUP_GEN) $(NAME) priv/setup_device.config setup_device

setup_backend:
	ERL_LIBS=$(PWD)/deps:$(ERL_LIBS):$(PWD) \
	$(SETUP_GEN) $(NAME) priv/setup_backend.config setup_backend

target_backend:
	ERL_LIBS=$(PWD)/deps:$(ERL_LIBS) \
	$(SETUP_GEN) $(NAME) priv/setup_backend.config setup -pz $(PWD)/ebin \
	-target rel_backend -vsn 0.1

target_device:
	ERL_LIBS=$(PWD)/deps:$(ERL_LIBS) \
	$(SETUP_GEN) $(NAME) priv/setup_device.config setup -pz $(PWD)/ebin \
	-target rel_device -vsn 0.1

#
# Start the backend server
#
run_backend: setup_backend
	erl -boot setup_backend/start -config setup_backend/sys 


#
# Start the backend device.
#
run_device: setup_device
	erl -boot setup_device/start -config setup_device/sys 


doc:
        REBAR_DOC=1 rebar skip_deps=true get-deps doc

clean:
	rebar clean
