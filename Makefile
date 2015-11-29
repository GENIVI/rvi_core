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

SRC_LIST=BUILD.md \
	CONFIGURE.md \
	doc \
	LICENSE \
	Makefile \
	README.md \
	rebar \
	rebar.config \
	rel \
	RELEASE.md \
	scripts/setup_gen \
	scripts/rvi.service \
	scripts/rvi_ctl \
	scripts/rvi_install.sh \
	python/*.py \
	components \
	ebin \
	src \
	TODO 

VERSION=0.4.0

all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar  compile

recomp:
	./rebar  compile skip_deps=true

clean:   
	./rebar clean

ubuntu_clean:
	rm -rf ./ubuntu_build

rpm_clean:
	rm -rf ./rpm/BUILD/* \
		./rpm/BUILDROOT/* \
		./rpm/RPMS/* \
		./rpm/SOURCES/* \
		./rpm/SRPMS/*

# Create a source tarball
src_tarball: clean
	tar czf ./rvi-$(VERSION).tgz $(SRC_LIST)
		components rvi_sample.config scripts/setup_rvi_node.sh src \
		TODO 

# Create a SOURCES tarball for RPM
rpm_tarball: clean rpm_clean 
	tar czf ./rvi-$(VERSION).tgz $(SRC_LIST) rpm
	mv ./rvi-$(VERSION).tgz ./rpm/SOURCES/

# Create an ubuntu 14.04 tarball
ubuntu_package: clean ubuntu_clean
	install --mode=0755 -d ./ubuntu_build
# Pack up all relevant files, and debian/,  necessary for a build.
# Add rvi-$(VERSION) at the beginning of each file so
# that theu get packed up into a correctly named subdirectory
# 
	tar czf ./ubuntu_build/rvi_$(VERSION).orig.tar.gz \
		--exclude-vcs --transform="s|^|./rvi-$(VERSION)/|" \
		$(SRC_LIST) \
		debian \
		rvi_ubuntu.config \
		scripts/rvi.init.ubuntu
	# Unpack the created tar file
	(cd ./ubuntu_build; tar xf rvi_$(VERSION).orig.tar.gz)
	# Descend into the unpacked directory and build.
	(cd ./ubuntu_build/rvi-$(VERSION); debuild -uc -us)

rpm:	rpmclean rpm_tarball 
	rpmbuild --define "_topdir $$PWD/rpm" -ba rpm/SPECS/rvi-$(VERSION).spec

install: deps compile
	./scripts/rvi_install.sh $(DESTDIR)/opt/rvi $(DESTDIR)/opt/rvi $(DESTDIR)/var/opt/log/rvi
	install --mode=0755 -d $(DESTDIR)/etc/opt/rvi/
	install --mode=0644 rvi_yocto.config $(DESTDIR)/etc/opt/rvi/rvi.config
