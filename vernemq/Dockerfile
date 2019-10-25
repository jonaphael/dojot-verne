FROM vernemq/vernemq:alpine-latest

USER root

COPY command.sh /


COPY scripts_tls/ /vernemq/scripts_tls
COPY vmq_dojot /vernemq/vmq_dojot.sh
COPY cert/ /vernemq/cert/

COPY vmq.acl etc/vmq.acl

RUN chmod 777  /vernemq/vmq_dojot.sh
RUN chmod 777 -R  /vernemq/scripts_tls
RUN chmod 777 -R  /vernemq/cert/

ENTRYPOINT ["/command.sh"]

CMD ["./vmq_dojot.sh"]

