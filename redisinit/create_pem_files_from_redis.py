
import redis
import conf
import logging
from thing import Thing


if __name__ == "__main__":

    format = "%(asctime)s: %(message)s"
    logging.basicConfig(format=format, level=logging.DEBUG, datefmt="%H:%M:%S")

    r = redis.Redis(host = conf.redis_host, port = conf.redis_port, db = 0)

    i = 0
    print ("PEM Keys:")
    for key in r.scan_iter(count=2):
        x = (r.hmget(name=key, keys="private_key"))
        i  = i + 1
        print (i)
        with open("cert_things/"+(key.decode("utf-8"))+".key", "w") as keyFile:
            keyFile.write((x[0]).decode("utf-8") )

    i = 0
    print ("PEM Crts:")
    for key in r.scan_iter(count=2):
        x = (r.hmget(name=key, keys="thing_certificate"))
        i  = i + 1
        print (i)
        with open("cert_things/"+(key.decode("utf-8"))+".crt", "w") as crtFile:
            crtFile.write((x[0]).decode("utf-8") )

