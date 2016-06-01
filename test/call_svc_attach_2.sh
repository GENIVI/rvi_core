#!/bin/sh
#   Call service with attachment
#

URL="http://localhost:9001"

if [ "$#" = "2" ]
then
    SVC=$1
    FILE=$2
else
    echo "Usage: $0 service file"
    exit 255
fi


if [ ! -f $FILE ]
then
    echo "File $1 is not readable."
    exit 255
fi

B="---//$(date +%s)//---"   # boundary delimiter

BASENAME=$(basename $FILE)

# Create a file containing the actual call
cat > /tmp/svc_attach.json <<EOF
{
    "jsonrpc": "2.0",
    "method": "message",
    "id": "1",
    "params":
    {
        "service_name": "${SVC}",
        "timeout": 20,
        "parameters": {"data":"file:${BASENAME}"}
    }
}
EOF

curl -F "file=@${FILE};filename=${BASENAME}" \
    -F "file=@/tmp/svc_attach.json;filename=body;type=application/json" $URL
echo
