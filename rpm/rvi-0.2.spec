Summary: Remote Vehicle Interaction Node
Name: rvi
Version: 0.2
Release: 1
# Copyright: Jaguar Land Rover -
License: Mozilla Public License v2
Vendor: Jaguar Land Rover
Group: Applications/System
Source: http://content.linuxfoundation.org/auto/downloads/rvi/rvi-0.2.tgz
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
%setup 

%build

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION
cp -ar * $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/opt/rvi-0.2
