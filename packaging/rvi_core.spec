Summary:    Remote Vehicle Interaction Node, running on top of Erlang,
Name:       rvi_core
Version:    0.5.1
Release:    1
Group:      App Framework/Application Communication
License:    Mozilla Public License 2.0
Source:     http://content.linuxfoundation.org/auto/downloads/rvi/rvi_core-0.5.1.tgz

BuildRequires:  make
BuildRequires:  glib2-devel
BuildRequires:  rpm
BuildRequires:  git
BuildRequires:  erlang
BuildRequires:  bluez-devel


%description
RVI Node running on Tizen. Needs erlang. See README.md

%prep
%setup -c rvi_core-$RPM_PACKAGE_VERSION

for i in $(find deps -name '*.app.src'); do sed 's/git/"1.0"/' < $i > $i.tmp; mv $i.tmp $i; done


make compile
# Create a tizen node if that is what we have.
./scripts/setup_rvi_node.sh -n rvi_tizen -c ./packaging/tizen.config
%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/share/rvi-$RPM_PACKAGE_VERSION
cp -ar rel/rvi_tizen $RPM_BUILD_ROOT/usr/share/rvi-$RPM_PACKAGE_VERSION
cp -ar python $RPM_BUILD_ROOT/usr/share/rvi-$RPM_PACKAGE_VERSION
mkdir -p $RPM_BUILD_ROOT/usr/lib/systemd/system/
mkdir -p $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/
install ./scripts/rvi.service $RPM_BUILD_ROOT/usr/lib/systemd/system/rvi.service 
ln -fsr $RPM_BUILD_ROOT/usr/lib/systemd/system/rvi.service \
    $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/rvi.service
ln -fsr $RPM_BUILD_ROOT/usr/share/rvi-$RPM_PACKAGE_VERSION/releases/$RPM_PACKAGE_VERSION/sys.config \
       $RPM_BUILD_ROOT/usr/share/rvi-$RPM_PACKAGE_VERSION/sys.config 
mkdir -p $RPM_BUILD_ROOT/home/app/content/Documents
echo "default_vin" > $RPM_BUILD_ROOT/home/app/content/Documents/vin

%clean
rm -rf $RPM_BUILD_ROOT

%files 
%manifest packaging/rvi_core.manifest 
%defattr(-,root,root)
%attr(644,app,users) /home/app/content/Documents/vin
/usr/lib/systemd/system/rvi.service 
/etc/systemd/system/multi-user.target.wants/rvi.service
/usr/share/rvi-0.5.1
