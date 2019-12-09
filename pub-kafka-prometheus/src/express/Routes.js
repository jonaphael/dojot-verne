const routes = require('express').Router();
const { logger } = require("@dojot/dojot-module-logger");
const util = require('util');

const PrometheusDojot = require('../PrometheusDojot');
const metrics = require('../Metrics');

const TAG = { filename: 'express/Routes' };

routes.get('/metrics', (req, res) => {

    res.set('Content-Type', PrometheusDojot.getRegisterContentType());

    PrometheusDojot.setMax(metrics.getMax());
    PrometheusDojot.setMin(metrics.getMin());
    PrometheusDojot.setAvg(metrics.getAvg());
    PrometheusDojot.setMedian(metrics.getMedian());
    PrometheusDojot.setStandardDeviation(metrics.getStandardDeviation());

    logger.debug(`Expose metrics ${util.inspect(metrics.getAllTimes(), { depth: null })}`, TAG);

    metrics.cleanAllTimes();

    res.end(PrometheusDojot.getRegisterMetrics());
});

module.exports = routes;