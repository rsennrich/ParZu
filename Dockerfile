FROM ubuntu:20.04

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -y install \
    git \
    swi-prolog \
    sfst \
    unzip \
    wget \
    python3 \
    python3-pexpect \
    python3-flask \
    python-is-python3

ADD https://api.github.com/repos/rsennrich/ParZu/git/refs/heads/master version.json
RUN git clone https://github.com/rsennrich/ParZu

RUN (cd ParZu; bash install.sh)

WORKDIR /ParZu
CMD python3 parzu_server.py --host 0.0.0.0
