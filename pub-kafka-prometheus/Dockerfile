FROM node:10.16.3-alpine

WORKDIR /opt/prometheus

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

COPY package.json .
COPY package-lock.json .

COPY app .
RUN npm install
ENTRYPOINT ["./entrypoint.sh"]
