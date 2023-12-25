FROM ubuntu
RUN apt-get update && \
    apt-get install -y tcpdump
CMD ["tcpdump", "-i", "any", "-nn", "-vvv", "-w", "/pcap/capture.pcap"]