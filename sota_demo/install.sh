#!/bin/sh
#
# Installer for sota_demo python code on a Tizen box.
#
# Copy this entire directory (sota_demo) over to the target machine
# and execute this script.
#
# Install jsonrpclib 
(cd jsonrpclib; python setup.py install)

# Install the code.
rm -r /opt/sota
mkdir -p /opt/sota

cp rvi_json_rpc_server.py /opt/sota
cp sota_device.py /opt/sota

# Setup systemd
mkdir -p $RPM_BUILD_ROOT/usr/lib/systemd/system/
mkdir -p $RPM_BUILD_ROOT/etc/systemd/system/multi-user.target.wants/
rm -f /usr/lib/systemd/system/sota.service
rm -f /etc/systemd/system/multi-user.target.wants/sota.service
rm -f /etc/systemd/system/multi-user.target.wants/sota.service
rm -f /usr/lib/systemd/system/sota.service
install ./sota.service /usr/lib/systemd/system/sota.service
ln -fsr /usr/lib/systemd/system/sota.service /etc/systemd/system/multi-user.target.wants/sota.service
