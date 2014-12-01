Summary:    Remote Vehicle Interaction - Big Data Demo packaging
Name:       big_data_demo
Version:    0.3.0
Release:    1
Group:      App Framework/Application Communication
License:    Mozilla Public License 2.0
Source:     http://content.linuxfoundation.org/auto/downloads/big_data_demo/big_data_demo-0.3.0.tgz

BuildRequires:  make
BuildRequires:  python
BuildRequires:  rpm
# BuildRequires:  git

%description 
Big Data Demo running on top of RVI


%prep
%setup -c big_data_demo-$RPM_PACKAGE_VERSION

%build

%install
# Install the code.

rm -fr $RPM_BUILD_ROOT/opt/big_data_demo-$RPM_PACKAGE_VERSION
mkdir -p $RPM_BUILD_ROOT/opt/big_data_demo-$RPM_PACKAGE_VERSION

cp ./big_data_demo/rvi_json_rpc_server.py $RPM_BUILD_ROOT/opt/big_data_demo-$RPM_PACKAGE_VERSION
cp ./big_data_demo/amb_dbus.py $RPM_BUILD_ROOT/opt/big_data_demo-$RPM_PACKAGE_VERSION
cp ./big_data_demo/data_logger.py $RPM_BUILD_ROOT/opt/big_data_demo-$RPM_PACKAGE_VERSION
cp ./big_data_demo/gps_collector.py $RPM_BUILD_ROOT/opt/big_data_demo-$RPM_PACKAGE_VERSION
cp ./big_data_demo/big_data_device.py $RPM_BUILD_ROOT/opt/big_data_demo-$RPM_PACKAGE_VERSION
cp -r ./big_data_demo/mod $RPM_BUILD_ROOT/opt/big_data_demo-$RPM_PACKAGE_VERSION

# Setup systemd
mkdir -p $RPM_BUILD_ROOT/usr/lib/systemd/system/
mkdir -p $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/
install ./big_data_demo/big_data.service $RPM_BUILD_ROOT/usr/lib/systemd/system/big_data.service
ln -fsr $RPM_BUILD_ROOT/usr/lib/systemd/system/big_data.service $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/big_data.service
###################

%post
/usr/bin/systemctl daemon-reload

%postun

%clean
rm -rf $RPM_BUILD_ROOT

%files 
%manifest packaging/big_data_demo.manifest 
%defattr(-,root,root)
/usr/lib/systemd/system/big_data.service 
/etc/systemd/system/multi-user.target.wants/big_data.service
/opt/big_data_demo-0.3.0
