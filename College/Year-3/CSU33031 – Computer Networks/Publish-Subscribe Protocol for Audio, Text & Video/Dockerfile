FROM ubuntu
WORKDIR /compnets
RUN apt-get update && \
    apt-get install -y net-tools netcat tcpdump inetutils-ping python3 python3-pip ffmpeg && \ 
    apt-get clean
COPY ./FrameSamples ./
COPY AudioSample.m4v ./
COPY broker.py ./
COPY consumer.py ./
COPY producer.py ./
RUN pip3 install Pillow
RUN pip3 install pydub
CMD ["/bin/bash"]
