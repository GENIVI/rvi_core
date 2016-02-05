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

.PHONY:	all deps compile clean rpm rpmclean test xref ci escript


SCRIPTS=scripts/setup_gen \
	scripts/author

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
	scripts/rvi_ctl.template \
	scripts/rvi_install \
	python/*.py \
	components \
	priv \
	ebin \
	src \
	deps \
	TODO 

VERSION=0.5.0

all: deps compile escript

deps:
	./rebar get-deps

compile:
	./rebar  compile

escript: compile ${SCRIPTS}

recomp:
	./rebar  compile skip_deps=true


scripts/setup_gen: deps/setup/setup_gen
	cp deps/setup/setup_gen scripts/

scripts/author: components/authorize/author
	cp components/authorize/author scripts/

components/authorize/author:
	(cd components/authorize && make escript)

clean:   rpmclean
	./rebar clean

ubuntu_clean:
	rm -rf ./ubuntu_build

debian_clean:
	rm -rf ./debian_build

rpm_clean:
	rm -rf ./rpm/BUILD/* \
		./rpm/BUILDROOT/* \
		./rpm/RPMS/* \
		./rpm/SOURCES/* \
		./rpm/SRPMS/*

xref: compile
	ERL_LIBS=$(PWD):$(PWD)/components:$(PWD)/deps rebar xref skip_deps=true

ci: xref test

test: compile escript
	./rebar ct skip_deps=true

# Create a SOURCES tarball for RPM
rpm_tarball: rpmclean clean
	tar czf /tmp/rvi_core-$(VERSION).tgz BUILD.md CONFIGURE.md doc \
		LICENSE Makefile README.md rebar rebar.config rel deps\
		RELEASE.md rpm scripts/setup_gen scripts/rvi \
		scripts/rvi.service scripts/rvi.sh \
		components priv/config/rvi_sample.config scripts/rvi_instball.sh src \

# Create an ubuntu package 
ubuntu_package: clean ubuntu_clean escript
	install --mode=0755 -d ./ubuntu_build

# Pack up all relevant files, and ubuntu/,  necessary for a build.
# Add rvi-$(VERSION) at the beginning of each file so
# that theu get packed up into a correctly named subdirectory
# 
	tar czf ./ubuntu_build/rvi_$(VERSION).orig.tar.gz \
		--exclude-vcs --transform="s|^|./rvi-$(VERSION)/|" \
		$(SRC_LIST) \
		ubuntu_template \
		scripts/rvi.init.ubuntu
	rm -rf ubuntu/missing-sources
# Unpack the created tar file
	(cd ./ubuntu_build; tar xf rvi_$(VERSION).orig.tar.gz)
# Move the ubuntu template to be the debian package
	mv ./ubuntu_build/rvi-$(VERSION)/ubuntu_template  ./ubuntu_build/rvi-$(VERSION)/debian
	install -d -m 0755 ./ubuntu_build/rvi-$(VERSION)/debian/missing-sources
# Descend into the unpacked directory and build.
	(cd ./ubuntu_build/rvi-$(VERSION); debuild -uc -us)

# Create a debian package 
debian_package: clean debian_clean escript
	install --mode=0755 -d ./debian_build

# Pack up all relevant files, and debian/,  necessary for a build.
# Add rvi-$(VERSION) at the beginning of each file so
# that theu get packed up into a correctly named subdirectory
# 
	tar czf ./debian_build/rvi_$(VERSION).orig.tar.gz \
		--exclude-vcs --transform="s|^|./rvi-$(VERSION)/|" \
		$(SRC_LIST) \
		debian_template \
		scripts/rvi.init.debian
	rm -rf debian/missing-sources
# Unpack the created tar file
	(cd ./debian_build; tar xf rvi_$(VERSION).orig.tar.gz)
# Move the debian template to be the debian package
	mv ./debian_build/rvi-$(VERSION)/debian_template  ./debian_build/rvi-$(VERSION)/debian
	install -d -m 0755 ./debian_build/rvi-$(VERSION)/debian/missing-sources
# Descend into the unpacked directory and build.
	(cd ./debian_build/rvi-$(VERSION); debuild -uc -us)


rpm:	rpmclean rpm_tarball 
	rpmbuild --define "_topdir $$PWD/rpm" -ba rpm/SPECS/rvi-$(VERSION).spec

install: deps compile
	 ./scripts/rvi_install \
		-k priv/keys/insecure_device_key.pem \
		-r priv/certificates/insecure_root_cert.crt \
		-d priv/certificates/insecure_device_cert.crt \
		-c priv/credentials/insecure_credential.jwt $(DESTDIR)/opt/rvi_core

	install -m 0755 -d $(DESTDIR)/etc/opt/rvi/
	install -m 0644 priv/config/rvi_sample.config $(DESTDIR)/etc/opt/rvi/rvi_sample.config
