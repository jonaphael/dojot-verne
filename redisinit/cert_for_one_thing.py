import uuid
import conf
import logging
from thing import Thing


if __name__ == "__main__":
    format = "%(asctime)s: %(message)s"
    logging.basicConfig(format=format, level=logging.DEBUG, datefmt="%H:%M:%S")

    thing_id = str(uuid.uuid4().hex)
    obj = Thing.Create_Thing(thing_id)   
    
    with open(thing_id+".key", "w") as keyFile:
        keyFile.write(str(obj['private_key']))
    
    with open(thing_id+".crt", "w") as crtFile:
        crtFile.write(str(obj['thing_certificate']))

    
