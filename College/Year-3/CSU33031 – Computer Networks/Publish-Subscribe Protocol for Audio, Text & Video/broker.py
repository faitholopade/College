#@Author: Faith Olopade

import socket

localIP = "0.0.0.0"
localPort = 50000
bufferSize = 65535

subscriptions = {}

UDPServerSocket = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM)
UDPServerSocket.bind((localIP, localPort))
print("Broker is up and listening")

while True:
    data, addr = UDPServerSocket.recvfrom(bufferSize)
    
    packet_type = data[0]

    if packet_type == 3:  # Subscription packet
        producer_id = data[1:7].decode()
        stream_no = data[7:8][0]
        stream_id = producer_id + str(stream_no) if stream_no != 255 else producer_id + "ALL"

        if stream_id not in subscriptions:
            subscriptions[stream_id] = []

        if addr not in subscriptions[stream_id]:
            subscriptions[stream_id].append(addr)

        print(f"Received subscription request from {addr} for stream {stream_id}.")
        # Send subscribe ack to the consumer
        UDPServerSocket.sendto(b'\x04', addr)
        continue
    
    header = data[:17]
    payload = data[17:]

    producer_id = header[1:7].decode()
    stream_no = header[7:8][0]
    frame_no = int.from_bytes(header[8:12], byteorder='big')
    payload_length = int.from_bytes(header[12:16], byteorder='big')


    stream_id = producer_id + str(stream_no)

    forwarded = False  # Flag to indicate if the data was forwarded

    for sub_stream_id in [stream_id, producer_id + "ALL"]:
        if sub_stream_id in subscriptions:
            for subscriber in subscriptions[sub_stream_id]:
                UDPServerSocket.sendto(data, subscriber)
                forwarded = True

    if not forwarded:
        print(f"No subscribers found for stream {stream_id}.")

    print(f"Received request to publish frame {frame_no} (Length: {payload_length} bytes) for stream {stream_id} from producer {producer_id}. Forwarded to subscribers.")


    print(f"Received request to publish frame {frame_no} (Length: {payload_length} bytes) for stream {stream_id} from producer {producer_id}. Forwarded to subscribers.")

    # Send publish ack to the producer
    UDPServerSocket.sendto(b'\x02', addr)
