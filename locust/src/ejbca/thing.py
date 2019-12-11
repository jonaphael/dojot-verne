import logging

from src.ejbca.certificate import Certificate

class Thing():
    def __init__(self, thing_id: str):
        try:
            self.cert = Certificate(thing_id)
            self.id = thing_id
            self.private_key = self.cert.key["pem"]
            self.certificate = self.cert.crt["pem"]

        except Exception as exception:
            logging.warning("Error: %s", str(exception))

    def renew_cert(self) -> None:
        """
        Renew a certificate.
        """
        self.cert.renew_cert()
