const Prometheus = require('prom-client')

const dojotLatency = new Prometheus.Gauge({
    name: 'Latency',
    help: 'Dojot maximum latency of IoTAgent Service',
    labelNames: ['axis', 'value'],
})

function setLatencyData() {
    setInterval(() => {
        dojotLatency.set({ axis: 'x', value: 'latency' }, 200);
        dojotLatency.set({ axis: 'y', value: 'connections' }, 1000);
    }, 100);
}


module.exports = { setLatencyData };