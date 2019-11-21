FROM node:10.16.3-alpine

WORKDIR /opt/prometheus

RUN apk add --no-cache g++ \
    make \
    bash \
    python \
    lz4-dev \
    musl-dev \
    ca-certificates \
    openssl-dev

COPY package.json .
COPY package-lock.json .

COPY . .
RUN npm install
ENTRYPOINT ["./entrypoint.sh"]