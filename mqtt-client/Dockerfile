FROM node:10.16.3-alpine

RUN apk --no-cache add \
      bash \
      g++ \
      ca-certificates \
      lz4-dev \
      musl-dev \
      cyrus-sasl-dev \
      openssl-dev \
      make \
      python \
      openssl \
      curl \
      jq

WORKDIR /opt/mqtt_client

COPY . .
RUN npm install
RUN npm run-script build
ENTRYPOINT ["./entrypoint.sh"]