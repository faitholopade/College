#@Author: Faith Olopade

import socket
import time
from PIL import Image
import os
from pydub import AudioSegment
from pydub.playback import play

VIDEO = 1
TEXT = 2
AUDIO = 3

brokerAddressPort = ("broker", 50000)
bufferSize = 65535

UDPClientSocket = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM)

producer_id = input("Enter producer ID (6 chars):")
stream_no = int(input("Enter stream number (0-255):"))
frame_no_video = 1
frame_no_text = 1
frame_no_audio = 1

def get_video_frame_path(frame_no):
    return f"./FrameSamples/frame{str(frame_no).zfill(3)}.png"

def get_audio_frame(frame_no):
    AUDIO_PATH = "./AudioSample.m4v"
    SEGMENT_DURATION = 1   # 1 second per segment

    audio = AudioSegment.from_file(AUDIO_PATH, format="mp4")
    start_time = (frame_no - 1) * SEGMENT_DURATION
    end_time = frame_no * SEGMENT_DURATION

    segment = audio[start_time:end_time]

    return segment.raw_data

while True:
    payload_content_type = input("Enter content type (video, text, audio):")
    
    if payload_content_type == "video":
        content_type_str = "video"
        content_type_byte = VIDEO.to_bytes(1, byteorder='big')
        with Image.open(get_video_frame_path(frame_no_video)) as img:
            with open(get_video_frame_path(frame_no_video), 'rb') as f:
                payload_bytes = f.read()
        current_frame_no = frame_no_video
        frame_no_video += 1
    elif payload_content_type == "text":
        content_type_str = "text"
        content_type_byte = TEXT.to_bytes(1, byteorder='big')
        text_content = input("Enter text content:")
        payload_bytes = text_content.encode()
        current_frame_no = frame_no_text
        frame_no_text += 1
    elif payload_content_type == "audio":
        content_type_str = "audio"
        content_type_byte = AUDIO.to_bytes(1, byteorder='big')
        payload_bytes = get_audio_frame(frame_no_audio)
        current_frame_no = frame_no_audio
        frame_no_audio += 1
    else:
        print("Invalid content type!")
        continue
    
    payload_length = len(payload_bytes).to_bytes(4, byteorder='big')
    header = b'\x01' + producer_id.encode() + stream_no.to_bytes(1, byteorder='big') + current_frame_no.to_bytes(4, byteorder='big') + payload_length + content_type_byte
    UDPClientSocket.sendto(header + payload_bytes, brokerAddressPort)
    print(f"Sent {content_type_str} frame {current_frame_no} to broker.")
    

    ack_data, _ = UDPClientSocket.recvfrom(bufferSize)
    if ack_data.startswith(b'\x02'):
        print("Received publish ack from broker")
    
    time.sleep(1)
