
import redis
import conf
import logging
import sys
from thing import Thing


if __name__ == "__main__":

    format = "%(asctime)s: %(message)s"
    logging.basicConfig(format=format, level=logging.DEBUG, datefmt="%H:%M:%S")


    try:
        r = redis.Redis(host = conf.redis_host, port = conf.redis_port, db = 0)
        r.ping()
    except Exception as exception:
        logging.error("Error while connecting to Redis.")
        sys.exit(1)


    try:
        logging.info("Creating key files...")
        for key in r.scan_iter(count=2):
            x = (r.hmget(name=key, keys="private_key"))
            with open("cert/"+(key.decode("utf-8"))+".key", "w") as keyFile:
                keyFile.write((x[0]).decode("utf-8") )
    except Exception as exception:
        logging.error("Error while saving key files: %s", str(exception))
        sys.exit(1)
    else:
        logging.info("Successfully created!")


    try:
        logging.info("Creating certificate files...")
        for key in r.scan_iter(count=2):
            x = (r.hmget(name=key, keys="thing_certificate"))
            with open("cert/"+(key.decode("utf-8"))+".crt", "w") as crtFile:
                crtFile.write((x[0]).decode("utf-8") )
    except Exception as exception:
        logging.error("Error while saving key files: %s", str(exception))
        sys.exit(1)
    else:
        logging.info("Successfully created!")

