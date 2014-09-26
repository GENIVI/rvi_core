Summary:    Remote Vehicle Interaction Node, running on top of Erlang,
Name:       rvi
Version:    0.2.0
Release:    1
Group:      Applications/System
License:    Mozilla Public License v2
Source:     http://content.linuxfoundation.org/auto/downloads/rvi/rvi-0.2.0.tgz

BuildRequires:  make
BuildRequires:  glib2-devel
BuildRequires:  rpm
BuildRequires:  git
BuildRequires:  erlang

%description
RVI Node running on Tizen. Needs erlang

%package devel
Summary:    RVI development headers
Group:      Development/Libraries
Requires:   %{name} = %{version}

%description devel
RVI development headers

%prep
%setup -c rvi-$RPM_PACKAGE_VERSION

%build
make deps
make compile
# Create a tizen node if that is what we have.
./scripts/setup_rvi_node.sh -n rvi-$RPM_PACKAGE_VERSION -c tizen.config
%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION
cp -ar rel/rvi-$RPM_PACKAGE_VERSION $RPM_BUILD_ROOT/opt/
mkdir -p $RPM_BUILD_ROOT/usr/lib/systemd/system/
mkdir -p $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/
install ./scripts/rvi.service $RPM_BUILD_ROOT/usr/lib/systemd/system/rvi.service 
ln -fsr $RPM_BUILD_ROOT/usr/lib/systemd/system/rvi.service \
    $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/rvi.service

ln -fsr $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION/releases/1/sys.config \
       $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION/sys.config 


%post
/usr/bin/systemctl daemon-reload

%postun

%clean
rm -rf $RPM_BUILD_ROOT

%files 
%manifest packaging/rvi.manifest 
%defattr(-,root,root)
/usr/lib/systemd/system/rvi.service 
/etc/systemd/system/multi-user.target.wants/rvi.service
/opt/rvi-0.2.0

