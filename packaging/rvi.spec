Summary:    Remote Vehicle Interaction Node, running on top of Erlang,
Name:       rvi
Version:    0.4.0
Release:    1
Group:      App Framework/Application Communication
License:    Mozilla Public License 2.0
Source:     http://content.linuxfoundation.org/auto/downloads/rvi/rvi-0.4.0.tgz

BuildRequires:  make
BuildRequires:  glib2-devel
BuildRequires:  rpm
BuildRequires:  git
BuildRequires:  erlang
BuildRequires:  bluez-devel


%description
RVI Node running on Tizen. Needs erlang. See README.md

%prep
%setup -c rvi-$RPM_PACKAGE_VERSION

%build
for i in $(find deps -name '*.app.src'); do sed 's/git/"1.0"/' < $i > $i.tmp; mv $i.tmp $i; done


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

ln -fsr $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION/releases/$RPM_PACKAGE_VERSION/sys.config \
       $RPM_BUILD_ROOT/opt/rvi-$RPM_PACKAGE_VERSION/sys.config 

%post
if [ ! -f /home/app/content/Documents/vin ]
then
    uuidgen > /home/app/content/Documents/vin
    chown app.users /home/app/content/Documents/vin
    echo "VIN created in /home/app/content/Documents/vin:"
    echo "   $(cat /home/app/content/Documents/vin)"
    
else
    echo "Will not touch existing VIN in /home/app/content/Documents/vin:"
    echo "   $(cat /home/app/content/Documents/vin)"
fi
/usr/bin/systemctl daemon-reload

%postun

%clean
rm -rf $RPM_BUILD_ROOT

%files 
%manifest packaging/rvi.manifest 
%defattr(-,root,root)
/usr/lib/systemd/system/rvi.service 
/etc/systemd/system/multi-user.target.wants/rvi.service
/opt/rvi-0.4.0
