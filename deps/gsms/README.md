
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

# Session behaviors

The gsms_plivo module is an example of a session behavior, designed to send and receive
SMSes via the http://plivo.com Voice/SMS service. To use it, you need to sign up for a
Plivo account, but for testing purposes, you can use the gsms_plivo_sim module to simulate
a Plivo service locally.

To test the Plivo behavior locally, you can try the following:

In one terminal window:

    $ make shell
    rebar get-deps
    ...
    rebar compile
    ...
    Eshell V5.10.4  (abort with ^G)
    1> gsms_plivo_sim:simtest().
    {ok,<0.41.0>}
    2>

This starts a simulator instance on port 9100 with two predefined accounts (services).
An example of a service:

    {s1, [{type, plivo_sim},
          {numbers, ["111"]},
          {uri, "http://localhost:9200"},
          {account, "acct1"},
          {auth, "auth1"}]}

The 'account' option corresponds to the Plivo "Auth ID", and 'auth' to the "Auth Token".
The 'numbers' list is needed in order for the simulator to route SMSes from one client to
another. The 'uri' denotes the client's callback URI.

To start clients 1 and 2 respectively:

    $ make shell
    rebar get-deps
    ...
    rebar compile
    ...
    Eshell V5.10.4  (abort with ^G)
    1> gsms_plivo:simtest(1).   % or simtest(2)
    <0.41.0>
    2>

Note that the clients have dbg trace enabled. Modify or copy the code to disable.
