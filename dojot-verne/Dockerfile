FROM erlio/docker-vernemq

EXPOSE 1883 8883 8080 44053 4369 8888 \
    9100 9101 9102 9103 9104 9105 9106 9107 9108 9109

USER root

RUN apt-get update && \
    apt-get -y install uuid-runtime

COPY command.sh /

COPY vmq_dojot.sh /vernemq

USER vernemq

ENTRYPOINT ["/command.sh"]

CMD ["./vmq_dojot.sh"]