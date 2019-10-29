FROM node:10.16.3-alpine

WORKDIR /opt/verne_webhooks

RUN apk add --no-cache g++ \
        make \
        bash \
        python \
        lz4-dev \
        musl-dev \
        ca-certificates \
        openssl-dev \
        tini

COPY package.json .
COPY package-lock.json .

RUN node -v
RUN npm -v

RUN npm install
COPY . .

RUN npm run-script build


ENTRYPOINT ["/sbin/tini", "--"]

EXPOSE 4000
CMD ["npm", "run", "webhook"]
