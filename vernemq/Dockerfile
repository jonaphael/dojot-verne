FROM vernemq/vernemq

USER root

RUN apt-get update && \
    apt-get -y install uuid-runtime && \
    apt-get -y install cron

COPY command.sh /

COPY vmq_dojot.sh /vernemq
COPY _retrieveCRLCertificate.sh /vernemq
COPY cert/ /vernemq/cert/
COPY vmq.acl etc/vmq.acl

USER vernemq

ENTRYPOINT ["/command.sh"]

CMD ["./vmq_dojot.sh"]

