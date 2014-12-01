Summary:    Remote Vehicle Interaction - SOTA Demo packaging
Name:       sota_demo
Version:    0.3.0
Release:    1
Group:      App Framework/Application Communication
License:    Mozilla Public License 2.0
Source:     http://content.linuxfoundation.org/auto/downloads/sota_demo/sota_demo-0.3.0.tgz

BuildRequires:  make
BuildRequires:  python
# BuildRequires:  glib2-devel
BuildRequires:  rpm
# BuildRequires:  git

%description 
SOTA Demo running on top of RVI


%prep
%setup -c sota_demo-$RPM_PACKAGE_VERSION

%build

%install
# Install the code.

rm -fr $RPM_BUILD_ROOT/opt/sota_demo-$RPM_PACKAGE_VERSION
mkdir -p $RPM_BUILD_ROOT/opt/sota_demo-$RPM_PACKAGE_VERSION

cp -r ./sota_demo/mod $RPM_BUILD_ROOT/opt/sota_demo-$RPM_PACKAGE_VERSION
cp ./sota_demo/rvi_json_rpc_server.py $RPM_BUILD_ROOT/opt/sota_demo-$RPM_PACKAGE_VERSION
cp ./sota_demo/sota_device.py $RPM_BUILD_ROOT/opt/sota_demo-$RPM_PACKAGE_VERSION

# Setup systemd
mkdir -p $RPM_BUILD_ROOT/usr/lib/systemd/system/
mkdir -p $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/
install ./sota_demo/sota.service $RPM_BUILD_ROOT/usr/lib/systemd/system/sota.service
ln -fsr $RPM_BUILD_ROOT/usr/lib/systemd/system/sota.service $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/sota.service
###################



%post
/usr/bin/systemctl daemon-reload

%postun

%clean
rm -rf $RPM_BUILD_ROOT

%files 
%manifest packaging/sota_demo.manifest 
%defattr(-,root,root)
/usr/lib/systemd/system/sota.service 
/etc/systemd/system/multi-user.target.wants/sota.service
/opt/sota_demo-0.3.0

