# INSTALL INSTRUCTIONS FOR UBUNTU 14.04 PRECISE

1. Install dependent libraries
sudo apt-get install python-simplejson python-jsonrpclib libwxgtk2.8-0

2. Download esl-erlang 18.2
wget ttps://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_18.2-1~ubuntu~precise_amd64.deb

3. Install esl-erlang
sudo dpkg -i esl-erlang_18.2-1~ubuntu~precise_amd64.deb

4. Install RVI
sudo dpkg -i rvi_0.5.0-1ubuntu1_amd64.deb

5. Setup device keys
Assumes that root key pair is already generated.
doc/rvi_protocol.md

6. Setup device X.509 Certificates

