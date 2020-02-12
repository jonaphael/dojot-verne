FROM confluentinc/cp-kafkacat

RUN apt update; apt install -y curl jq expect

WORKDIR /opt/loopback/

COPY ./bin/entrypoint.sh ./bin/entrypoint.sh

ENTRYPOINT [ "./bin/entrypoint.sh" ]