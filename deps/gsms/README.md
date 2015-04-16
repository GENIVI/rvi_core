
# Start

# Send SMS

    gsms_router:send([{addr,"<number>"}], "Hello").

# Receive SMS

    {ok,Ref} = gsms_router:subscribe([]).
    Pdu = receive {gsms,Ref,Pdu1} -> Pdu1 end.
    Text = Pdu#gsms_deliver_pdu.ud.


# Raspberry pi setup

Setup internal serial port /dev/ttyAMA0 for use as
modem or other serial com line then ttyAMA0 must be 
disable in /etc/inittab

    # comment out in /etc/inittab
    # T0:23:respawn:/sbin/getty -L ttyAMA0 115200 vt100

Also remove rerences to ttyAMA0 in /boot/cmdline.txt
If cmdline looks like

    dwc_otg.lpm_enable=0 console=ttyAMA0,115200 kgdboc=ttyAMA0,115200 console=tty1 root=/dev/mmcblk0p2 rootfstype=ext4 elevator=deadline rootwait

After change it should look like

    dwc_otg.lpm_enable=0 console=tty1 root=/dev/mmcblk0p2 rootfstype=ext4 elevator=deadline rootwait

