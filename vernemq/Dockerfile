FROM vernemq/vernemq:alpine-latest

USER root

COPY command.sh /

COPY vmq_dojot.sh /vernemq
COPY _retrieveCRLCertificate.sh /vernemq
COPY cert/ /vernemq/cert/
COPY vmq.acl etc/vmq.acl

RUN chmod 777  /vernemq/vmq_dojot.sh
RUN chmod 777  /vernemq/_retrieveCRLCertificate.sh
RUN chmod 777 -R  /vernemq/cert/

ENTRYPOINT ["/command.sh"]

CMD ["./vmq_dojot.sh"]

