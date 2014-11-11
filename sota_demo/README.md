# SETUP INSTRUCTIONS FOR SOTA DEMO ON TIZEN BOX #

## SETUP

All files are available at rvi@rvi1.nginfotpdx.net:sota_demo/

Flash the tizen image TizenIVI30_APR22_AGL_19SEP2014.raw.gz

### Install RVI 
Once the new Tizen image has booted, install the RVI 0.3.0 rpm:

    rpm -i rvi-0.3.0-1.i686.rpm

### Set Tizen box VIN number

Edit the RVI config file to install a VIN number.

    vi /opt/rvi-0.3.0/sys.config
	
Append the VIN number to the end of the node_service_prefix value:

Before:

      {node_service_prefix,"jlr.com/vin/"},

After:

      {node_service_prefix,"jlr.com/vin/9UYA31581L000000"},

Save the sys.config and exit with ```<ESC><ESC>ZZ```

### Install the new home screen

Install the updated home screen: intelPoc10.HomeScreen.wgt.20141025_1

    wrt-installer -un intelPoc10.HomeScreen
    wrt-installer -i intelPoc10.HomeScreen.wgt.20141027_1

### Install the SOTA device agent

Unpack the sota_demo.tgz file to the Tizen box and unpack it:

    tar xzf sota_demo_0.3.0.tgz

Install the sota demo on the Tizen box.

    cd sota_demo
    sh install.sh


## RUNNING

Reboot the Tizen box to bring up the RVI node and the SOTA demo



