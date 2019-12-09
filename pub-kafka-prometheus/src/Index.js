const { logger } = require("@dojot/dojot-module-logger");
const ExpressApp = require("./express/App");
const KafkaMesseger = require("./KafkaMesseger");
const config = require('../Config');

const TAG = { filename: 'Index' };

logger.setLevel(config.app.log_level);

const expressApp = new ExpressApp();
const kafkaMesseger = new KafkaMesseger();

try {

    logger.info(`Starting Dojot Prometheus...`, TAG);

    expressApp.init();
    kafkaMesseger.init();

    logger.info(`... Dojot Prometheus initialized.`, TAG);

} catch (error) {
    logger.error(`Caught an error: ${error}`, TAG);
    expressApp.stopApp();
}
