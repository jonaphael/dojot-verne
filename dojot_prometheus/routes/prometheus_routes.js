const Prometheus = require('prom-client')

let prometheusRoute = (app) => {

    // Runs before each requests
    app.use((req, res, next) => {
        res.locals.startEpoch = Date.now()
        next()
    });

    app.get('/metrics', (req, res) => {
        res.set('Content-Type', Prometheus.register.contentType);
        res.end(Prometheus.register.metrics());
    });
}

module.exports = prometheusRoute
