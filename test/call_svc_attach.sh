#!/bin/sh
#   Call service with attachment
#

URL="http://localhost:8801"

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

cat > /tmp/svc_attach.json <<EOF
$(printf "%b" "\r\n")--${B}$(printf "%b" "\r\n")
Content-Type: application/json$(printf "%b" "\r\n")
Content-ID: body
{
    "jsonrpc": "2.0",
    "method": "message",
    "id": "1",
    "params":
    {
        "service_name": "${SVC}",
        "parameters": [ {"data":"file:${BASENAME}"} ]
    }
}
$(printf "%b" "\r\n")
EOF

cat > /tmp/svc_attach_f1.txt <<EOF
--${B}$(printf "%b" "\r\n")
Content-Type: application/octet-stream$(printf "%b" "\r\n")
Content-ID: ${BASENAME}$(printf "%b" "\r\n")
EOF

cat > /tmp/svc_attach_f2.txt <<EOF
$(printf "%b" "\r\n")--${B}--$(printf "%b" "\r\n")
EOF

# Create a file containing the actual call

curl -H "Content-Type: multipart/related; boundary=${B}" \
  --data-binary @/tmp/svc_attach.json \
  --data-binary @/tmp/svc_attach_f1.txt \
  --data-binary @${FILE} \
  --data-binary @/tmp/svc_attach_f2.txt $URL
#curl -H "Content-Type: multipart/related" -F file="@${FILE};filename=$BASENAME" --form "type:application/json;data=@/tmp/svc_attach.json" $URL
echo
