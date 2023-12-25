import sys
import socket
import json
import constants as const

# Assuming the routers know the addresses of the next routers
next_router_addresses = {
    'router1': ('router2', const.ROUTER_PORT),
    'router2': ('router3', const.ROUTER_PORT),
    'router3': None  # Router 3 is in the same network as endpoint 2
}

def main(argv):
    localIP = "0.0.0.0"
    bufferSize = 65535
    forwarding_table = {}
    comms = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM)
    comms.bind((localIP, const.ROUTER_PORT))

    print(f"Router {argv[0]}: Up and listening")

    while True:
        bytesAddressPair = comms.recvfrom(bufferSize)
        message = bytesAddressPair[0]
        address = bytesAddressPair[1]
        message_contents = json.loads(message.decode())

        print(f"Router {argv[0]}: Received packet - {message_contents} from {address}")

        # Handling message with 'destination' key
        if 'destination' in message_contents:
            if message_contents['destination'] in forwarding_table:
                next_hop = forwarding_table[message_contents['destination']]
                comms.sendto(message, next_hop)
                print(f"FORWARD: Router {argv[0]}: Forwarding packet {message_contents} to {next_hop}")
            elif argv[0] == 'router3':
                destination = 'endpoint2' if message_contents['destination'] == 'endpoint2' else 'endpoint4'
                reply = json.dumps({'destination_address': (destination, const.ROUTER_PORT)}).encode()
                comms.sendto(reply, address)
                print(f"REPLY: Router {argv[0]}: Replied with destination address {destination} to {address}")
            else:
                next_router = next_router_addresses.get(argv[0])
                if next_router:
                    comms.sendto(message, next_router)
                    print(f"BROADCAST: Router {argv[0]}: Broadcasting packet {message_contents} to next router: {next_router[0]}")

if __name__ == "__main__":
    main(sys.argv[1:])