Summary: Remote Vehicle Interaction Node
Name: rvi
Version: 0.2.1
Release: 1
# Copyright: Jaguar Land Rover -
License: Mozilla Public License v2
Vendor: Jaguar Land Rover
Group: Applications/System
Source: http://content.linuxfoundation.org/auto/downloads/rvi/rvi-0.2.1.tgz
Buildroot: /var/tmp/%{name}-buildroot

# Requires: 

%description
The RVI project sets up a P2P network allowing services on multiple
nodes to access each other through a number of different protocols
and data channels. The entire system is designed to be adapted
and rewritten to fullfil the requirements of the target production
environment.

# 
%prep
%setup -c rvi-$RPM_PACKAGE_VERSION

%build
make deps
make compile
# Create a sample config.
./scripts/setup_rvi_node.sh -n rvi-$RPM_PACKAGE_VERSION -c rvi_sample.config

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION
cp -ar rel/rvi-$RPM_PACKAGE_VERSION $RPM_BUILD_ROOT/opt/

# Create a standard debian setup for start/stop.
mkdir -p $RPM_BUILD_ROOT/etc/init.d
mkdir -p $RPM_BUILD_ROOT/etc/rc0.d
mkdir -p $RPM_BUILD_ROOT/etc/rc1.d
mkdir -p $RPM_BUILD_ROOT/etc/rc2.d
mkdir -p $RPM_BUILD_ROOT/etc/rc3.d
mkdir -p $RPM_BUILD_ROOT/etc/rc4.d
mkdir -p $RPM_BUILD_ROOT/etc/rc5.d
mkdir -p $RPM_BUILD_ROOT/etc/rc6.d
install ./scripts/rvi $RPM_BUILD_ROOT/etc/init.d
ln -fsr $RPM_BUILD_ROOT/etc/init.d/rvi $RPM_BUILD_ROOT/etc/rc0.d/K20-rvi
ln -fsr $RPM_BUILD_ROOT/etc/init.d/rvi $RPM_BUILD_ROOT/etc/rc1.d/K20-rvi
ln -fsr $RPM_BUILD_ROOT/etc/init.d/rvi $RPM_BUILD_ROOT/etc/rc2.d/S50-rvi
ln -fsr $RPM_BUILD_ROOT/etc/init.d/rvi $RPM_BUILD_ROOT/etc/rc3.d/S50-rvi
ln -fsr $RPM_BUILD_ROOT/etc/init.d/rvi $RPM_BUILD_ROOT/etc/rc4.d/S50-rvi
ln -fsr $RPM_BUILD_ROOT/etc/init.d/rvi $RPM_BUILD_ROOT/etc/rc5.d/S50-rvi
ln -fsr $RPM_BUILD_ROOT/etc/init.d/rvi $RPM_BUILD_ROOT/etc/rc6.d/K20-rvi

# Make global config file easier to access.
ln -fsr $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION/releases/1/sys.config \
       $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION/sys.config 
%post

%clean
rm -rf $RPM_BUILD_ROOT

%files 
%defattr(-,root,root)
/etc/init.d
/etc/rc0.d
/etc/rc1.d
/etc/rc2.d
/etc/rc3.d
/etc/rc4.d
/etc/rc5.d
/etc/rc6.d
/opt/rvi-0.2.1
