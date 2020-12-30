# To build docker image from file run: docker build -t mrjp .
# Below runs docker in interactive mode and attach volumes
# docker run --rm -v /path/to/local/workspace:/workspace -it mrjp /bin/bash

FROM ubuntu:18.04

ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && \
    apt-get install -y gcc && \
    apt-get install -y gcc-multilib&&  \
    rm -rf /var/lib/apt/lists/* \