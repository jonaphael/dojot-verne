const dojotModule = require('@jonaphael/dojot-module');

const { logger } = require('@dojot/dojot-module-logger');
const app = require('./app');
const soap = require('../lib/dojot_soap');
const config = require('../src/config');


const TAG = { filename: 'index' };


const dojotConfig = dojotModule.Config;

/* Creating the client */
/* WSDL url */
const url = config.soap.wsdlAddr;
const { caCrt } = config.soap;
const p12File = config.soap.clientP12;
const password = config.soap.clientPass;

const clientEJBCA = new soap.SoapClient(url, caCrt, p12File, password);
const messenger = new dojotModule.Messenger('ejbca', config.messenger);


app.initApp(clientEJBCA, messenger, dojotConfig).catch((err) => {
  logger.error(`Caught an error: ${err}`, TAG);
  app.stopApp();
});
