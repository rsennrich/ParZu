FROM ubuntu:16.04

RUN apt-get update && apt-get -y install \
    git \
    swi-prolog \
    sfst \
    unzip \
    wget \
    python \
    python-pexpect \
    python-flask

ADD https://api.github.com/repos/rsennrich/ParZu/git/refs/heads/master version.json
RUN git clone https://github.com/rsennrich/ParZu

RUN (cd ParZu; bash install.sh)

WORKDIR /ParZu
CMD python parzu_server.py --host 0.0.0.0
