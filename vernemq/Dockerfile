FROM vernemq/vernemq:1.10.0-alpine

USER root

ENV SUPERCRONIC_URL=https://github.com/aptible/supercronic/releases/download/v0.1.9/supercronic-linux-amd64 \
    SUPERCRONIC=supercronic-linux-amd64 \
    SUPERCRONIC_SHA1SUM=5ddf8ea26b56d4a7ff6faecdd8966610d5cb9d85

RUN curl -fsSLO "$SUPERCRONIC_URL"
RUN echo "${SUPERCRONIC_SHA1SUM}  ${SUPERCRONIC}" | sha1sum -c -
RUN chmod +x "$SUPERCRONIC"
RUN mv "$SUPERCRONIC" "/usr/local/bin/${SUPERCRONIC}"
RUN ln -s "/usr/local/bin/${SUPERCRONIC}" /usr/local/bin/supercronic

COPY command.sh /

COPY scripts_tls/ /vernemq/scripts_tls
COPY vmq_dojot.sh /vernemq/vmq_dojot.sh
COPY cert/ /vernemq/cert/

COPY plugins/dojot_acl_plugin/_build /vernemq/dojot_acl_plugin/
COPY plugins/dojot_disconnect_plugin/_build /vernemq/dojot_disconnect_plugin

RUN chmod 777  /vernemq/vmq_dojot.sh
RUN chmod 777 -R  /vernemq/scripts_tls
RUN chmod 777 -R  /vernemq/cert/

ENTRYPOINT ["/command.sh"]

CMD ["./vmq_dojot.sh"]

