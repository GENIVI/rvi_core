#
# Makefile for the RVI node.
# 
#

.PHONY:	all deps compile setup clean doc setup_backend setup_device rpm


VERSION=0.2
SETUP_GEN=./deps/setup/setup_gen



all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar  compile

recomp:
	./rebar  compile skip_deps=true

clean:
	./rebar clean

rpm:	deps compile
	./setup_rvi_node.sh -n rvi-$(VERSION) -c rvi_sample.config
	(cd rel; tar czf ../rpm/SOURCES/rvi-$(VERSION).tgz rvi-$(VERSION))
	rpmbuild --define "_topdir $$PWD/rpm" -ba rpm/rvi-0.2.spec
