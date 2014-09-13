#
# Makefile for the RVI node.
# 
#

.PHONY:	all deps compile clean rpm rpmclean


VERSION=0.2
SETUP_GEN=./deps/setup/setup_gen



all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar  compile

recomp:
	./rebar  compile skip_deps=true

clean:   rpmclean
	./rebar clean

rpmclean:
	rm -rf ./rpm/BUILD ./rpm/BUILDROOT ./rpm/RPM ./rpm/RPMS
	rm -rf ./rpm/SOURCES ./rpm/SPECS ./rpm/SRPM ./rpm/SRPMS

rpm:	deps compile
	./setup_rvi_node.sh -n rvi-$(VERSION) -c rvi_sample.config
	mkdir -p ./rpm/BUILD ./rpm/BUILDROOT ./rpm/RPM ./rpm/RPMS
	mkdir -p ./rpm/SOURCES ./rpm/SPECS ./rpm/SRPM ./rpm/SRPMS
	(cd rel; tar czf ../rpm/SOURCES/rvi-$(VERSION).tgz rvi-$(VERSION))
	rpmbuild --define "_topdir $$PWD/rpm" -ba rpm/rvi-0.2.spec
