#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Makefile for the RVI node.
# 

.PHONY:	all deps compile clean rpm rpmclean test


VERSION=0.4.0

all: deps compile escript

deps:
	./rebar get-deps

compile:
	./rebar  compile

escript: compile
	(cd components/authorize && make escript)

recomp:
	./rebar  compile skip_deps=true

clean:   rpmclean
	./rebar clean

rpmclean:
	rm -rf ./rpm/BUILD/* \
		./rpm/BUILDROOT/* \
		./rpm/RPMS/* \
		./rpm/SOURCES/* \
		./rpm/SRPMS/*

test: compile
	rebar ct

# Create a SOURCES tarball for RPM
rpm_tarball: rpmclean clean
	tar czf /tmp/rvi_core-$(VERSION).tgz BUILD.md CONFIGURE.md doc \
		LICENSE Makefile README.md rebar rebar.config rel \
		RELEASE.md rpm scripts/setup_gen scripts/rvi \
		scripts/rvi.service scripts/rvi_node.sh  components \
		rvi_sample.config scripts/setup_rvi_node.sh src \
		tizen.config TODO 
	mv /tmp/rvi-$(VERSION).tgz ./rpm/SOURCES/


rpm:	rpm_tarball
	rpmbuild --define "_topdir $$PWD/rpm" -ba rpm/SPECS/rvi-$(VERSION).spec
