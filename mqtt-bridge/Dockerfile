FROM node:10.16.3-alpine

WORKDIR /opt/mqtt_client

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

RUN apk add --no-cache --virtual .build-deps gcc zlib-dev libc-dev bsd-compat-headers py-setuptools bash

COPY package.json .
COPY package-lock.json .

RUN npm install
COPY . .

ENTRYPOINT ["./entryPoint.sh"]