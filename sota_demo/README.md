# SETUP INSTRUCTIONS FOR SOTA DEMO ON TIZEN BOX #

## SETUP

All files are available at rvi@rvi1.nginfotpdx.net:sota_demo/

Flash the tizen image TizenIVI30_APR22_AGL_19SEP2014.raw.gz

Once up, install the RVI 0.2.2 rpm:
rpm -i rvi-0.2.2-1.i686.rpm

Install the updated home screen: intelPoc10.HomeScreen.wgt.20141025_1
    wrt-installer -un intelPoc10.HomeScreen
    wrt-installer -i intelPoc10.HomeScreen.wgt.20141025_1

Unpack the sota_demo.tgz file to the Tizen box and unpack it:
    tar xzf sota_demo.tgz

Install the sota demo on the Tizen box.
    cd sota_demo
	sh install.sh


## RUNNING

Reboot the Tizen box to bring up the RVI node and the SOTA demo



