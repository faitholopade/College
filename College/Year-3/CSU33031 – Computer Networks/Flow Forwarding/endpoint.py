import sys
import socket
import random
import json
import constants as const

def generate_address():
    return ':'.join(['%02x' % random.randint(0, 255) for _ in range(4)])

def main(argv):
    localIP = "0.0.0.0"
    bufferSize = 65535
    endpoint_address = generate_address()
    router_addr = (argv[0], const.ROUTER_PORT)
    comms = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM)

    # Using numbers for source and destination
    endpoint_num = int(argv[1])
    destination_num = 2 if endpoint_num == 1 else 4

    data_packet = {
        'source': endpoint_num,
        'destination': destination_num,
        'data': f'Hello from endpoint {endpoint_num}'
    }
    data = json.dumps(data_packet).encode()
    
    print(f"DEST: Endpoint {endpoint_num} ({endpoint_address}): Sending packet {data_packet} to router {argv[0]}")
    comms.sendto(data, router_addr)

    # Receiving the response from the router
    response_data, addr = comms.recvfrom(bufferSize)
    response = json.loads(response_data.decode())

    print(f"Endpoint {endpoint_num} ({endpoint_address}): Received response from router {addr}: {response}")

    if 'destination_address' in response:
        destination_addr = tuple(response['destination_address'])
        print(f"Endpoint {endpoint_num} ({endpoint_address}): Sending data to {destination_addr}")
        comms.sendto(data, destination_addr)

        ack_data, ack_addr = comms.recvfrom(bufferSize)
        print(f"ACK: Endpoint {endpoint_num} ({endpoint_address}): Received acknowledgment from {ack_addr}")

if __name__ == "__main__":
    main(sys.argv[1:])
