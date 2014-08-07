#!/bin/sh
#  Create new accounts, like the ga account
#  But other accounts may be create
. $HOME/.exodmrc

  #if [ $# != 2 ]
#then
#    echo "Usage: $0 temperature"
#    exit 255
#fi
# the password (actually erlang node cookie) must be 100% hidden
# so this is only for testing!!!!!

URL=http://localhost:8811
curl -u $USER_AUTH -k -X POST  $URL -d @- << EOF
{
    "jsonrpc": "2.0",
    "method": "message",
    "id": "1",
    "params":
    {
      "calling_service": "hvac_app",
      "target": "jlr.com/backend/hvac/subscribe",
      "timeout": 1405099531,
      "parameters": [ 
        { "vin": 1234 },
        { "subscribing_service": "jlr.com/vin/1234/hvac/updates" }
      ]
    }
}
EOF
