#@Author: Faith Olopade

import socket

VIDEO = 1
TEXT = 2
AUDIO = 3

localIP = "0.0.0.0"
localPort = 50000
bufferSize = 65535

brokerAddressPort = ("broker", 50000)

UDPClientSocket = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM)
UDPClientSocket.bind((localIP, localPort))

producer_id = input("Enter producer ID (6 chars):")
stream_input = input("Enter stream number (0-255) or 'ALL' for all streams:")
stream_no = 255 if stream_input.upper() == 'ALL' else int(stream_input)

subscribe_msg = b'\x03' + producer_id.encode() + stream_no.to_bytes(1, byteorder='big')
UDPClientSocket.sendto(subscribe_msg, brokerAddressPort)
print("Subscription request packet sent to broker. Waiting for frames...")

while True:
    data, _ = UDPClientSocket.recvfrom(bufferSize)

    packet_type = data[0]

    if packet_type == 4:  # Subscribe ack
        print("Received subscribe ack from broker")
        continue

    header = data[:17]  # Adjusted to match the header size
    payload = data[17:]

    producer_id = header[1:7].decode()
    stream_no = header[7:8]
    frame_no = int.from_bytes(header[8:12], byteorder='big')
    payload_length = int.from_bytes(header[12:16], byteorder='big')  # Adjusted the length to 4 bytes
    content_type = header[16]
    content_type_str = "video" if content_type == VIDEO else ("text" if content_type == TEXT else "audio")

    # Displaying the frame data
    frame_sample = payload[:10] + b'...' if len(payload) > 10 else payload

    print(f"Received {content_type_str} frame {frame_no} (Length: {payload_length} bytes) from stream {stream_no} of producer {producer_id}. Data: {frame_sample}")
