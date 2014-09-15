Summary: Remote Vehicle Interaction Noed
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
%setup -c rvi-$RPM_PACKAGE_VERSION

%build
make deps
make compile
# Create a tizen node if that is what we have.
if dmesg | grep -q Tizen; then
    ./scripts/setup_rvi_node.sh -n rvi-$RPM_PACKAGE_VERSION -c tizen.config
else
    ./scripts/setup_rvi_node.sh -n rvi-$RPM_PACKAGE_VERSION -c rvi_sample.config
fi
%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION
cp -ar rel/rvi-$RPM_PACKAGE_VERSION $RPM_BUILD_ROOT/opt/

# If we are installing on Tizen, use the systemd setup
# If not on Tizen, assume std debian setup
if dmesg | grep -q Tizen; then
    mkdir -p $RPM_BUILD_ROOT/usr/lib/systemd/system/
    mkdir -p $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/
    install ./scripts/rvi.service $RPM_BUILD_ROOT/usr/lib/systemd/system/rvi.service 
    ln -fsr $RPM_BUILD_ROOT/usr/lib/systemd/system/rvi.service \
	    $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/rvi.service
    echo "/usr/lib/systemd/system/rvi.service 
          /etc/systemd/system/multi-user.target.wants/rvi.service" > file_list.txt
else
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
    
    echo "/etc/init.d
    /etc/rc0.d
    /etc/rc1.d
    /etc/rc2.d
    /etc/rc3.d
    /etc/rc4.d
    /etc/rc5.d
    /etc/rc6.d" > file_list.txt

fi


# Make global config file easier to access.
ln -fsr $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION/releases/1/sys.config \
       $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION/sys.config 
%post
if dmesg | grep -q Tizen; then
    /usr/bin/systemctl daemon-reload
fi

%clean
rm -rf $RPM_BUILD_ROOT

%files -f file_list.txt
%defattr(-,root,root)
/opt/rvi-0.2
