FROM node:10.16.3-alpine

WORKDIR /opt/verne_webhooks

COPY . .
RUN npm install
RUN npm run-script build

RUN apk add --no-cache tini

ENTRYPOINT ["/sbin/tini", "--"]

EXPOSE 4000
CMD ["npm", "run", "webhook"]
