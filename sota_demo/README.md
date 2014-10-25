# SETUP INSTRUCTIONS FOR TIZEN BOX #

## SETUP

All files are available at rvi@rvi1.nginfotpdx.net:sota_demo

Flash the tizen image TizenIVI30_APR22_AGL_19SEP2014.raw.gz

Once up, install the RVI 0.2.1 rpm: rvi-0.2.1-1.i686.rpm

Install the updated home screen: intelPoc10.HomeScreen.wgt.20141024_3
wrt-installer -un intelPoc10.HomeScreen
wrt-installer -i intelPoc10.HomeScreen.wgt.20141024_3

Upload the sota_device files: rvi_json_rpc_server.py  sota_device.py

Make sure that jsonrpclib is installed on the Tizen box: https://github.com/joshmarshall/jsonrpclib

## RUNNING

Reboot the Tizen box to bring up the RVI node

Stop the home screen app:
    wrt-launcher -k intelPoc10.HomeScreen

Start the sota device code on the Tizen box
    python sota_device.py http://localhost:8811

On the rvi1 node start the sota server
    python sota_server.py http://rvi1.nginfotpdx.net:8801

Queue a file on the sota server
    4711 SOTA_poster_v2.pdf
	
On the tizen box, start the homescreen:
	wrt-launcher -s intelPoc10.HomeScreen

Click OK for the package.


