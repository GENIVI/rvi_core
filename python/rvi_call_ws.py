import websocket
import time
import sys
import getopt
import json

opts, args = getopt.getopt(sys.argv[1:], "n:")

host = 'ws://localhost:8808'

for o, a in opts:
    if o == "-n":
        host = a
    else:
        usage()
if len(args) < 1:
    usage()

i = 0
service = args[0]
rvi_args = {}
for i in args[1:]:
    print i
    [k, v] = i.split('=')
    rvi_args[k] = v

ws = websocket.create_connection(host)

print "RVI Node:         ", host
print "Service:          ", service
print "args:             ", rvi_args

payload = {}
payload['jsonrpc'] = "2.0"
payload['params'] = {'service_name':service, 'timeout':(int(time.time())+60), 'parameters':rvi_args}
payload['id'] = "1"
payload['method'] = 'message'

ws.send(json.dumps(payload))