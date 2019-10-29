import logging
from cert import Certificate

class Thing():           
    @staticmethod
    def Create_Thing(thing_id):
        try:
            certificate = Certificate(thing_id)
            thing2 = {}
            thing2['thing_id'] = thing_id
            thing2['private_key'] = certificate.keyPEM
            thing2['thing_certificate'] = certificate.crtPEM

            return thing2
        except Exception as error:
            logging.warning(f"Error: {error}")

