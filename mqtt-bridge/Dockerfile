FROM node:10.16.3-alpine

WORKDIR /opt/mqtt-client

RUN apk add --no-cache g++ \
        make \
        bash \
        python \
        lz4-dev \
        musl-dev \
        ca-certificates \
        openssl-dev \
        tini 

RUN apk add --no-cache --virtual .build-deps gcc zlib-dev libc-dev bsd-compat-headers py-setuptools bash


COPY package.json .
COPY package-lock.json .

RUN node -v
RUN npm -v

RUN npm install
COPY . .

ENTRYPOINT ["/sbin/tini", "--"]
CMD ["npm", "run", "start"]
