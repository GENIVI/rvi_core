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

.PHONY:	all deps compile clean rpm rpmclean


VERSION=0.4.0

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
	rm -rf ./rpm/BUILD/* \
		./rpm/BUILDROOT/* \
		./rpm/RPMS/* \
		./rpm/SOURCES/* \
		./rpm/SRPMS/*

# Create a SOURCES tarball for RPM
rpm_tarball: rpmclean clean
	tar czf /tmp/rvi-$(VERSION).tgz BUILD.md CONFIGURE.md doc \
		LICENSE Makefile README.md rebar rebar.config rel \
		RELEASE.md rpm scripts/setup_gen scripts/rvi \
		scripts/rvi.service scripts/rvi_node.sh  components \
		rvi_sample.config scripts/setup_rvi_node.sh src \
		tizen.config TODO 
	mv /tmp/rvi-$(VERSION).tgz ./rpm/SOURCES/


rpm:	rpm_tarball
	rpmbuild --define "_topdir $$PWD/rpm" -ba rpm/SPECS/rvi-$(VERSION).spec

install: # deps compile
	@echo "Creating release in /opt/rvi"
	@rm -rf rvi rel/rvi
	@./scripts/setup_rvi_node.sh -c rvi_sample.config  -n rvi > /dev/null
	@mkdir -p /etc/opt/rvi/
	@mv ./rvi/sys.config /etc/opt/rvi
	@cp -r rel/rvi /opt
	@ln -s /etc/opt/rvi/sys.config /opt/rvi/sys.config
	@cp ./scripts/setup_gen /opt/rvi
	@cp ./scripts/start_rvi.sh /opt/rvi/
	@mkdir -p /opt/rvi/setup/ebin
	@cp deps/setup/ebin/* /opt/rvi/setup/ebin
	@echo "Done"
