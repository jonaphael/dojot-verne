import re
import requests
import json
from OpenSSL import crypto
import conf


class Certificate:

    def __init__(self, thingID):
        self.cName = thingID
        self.keyRAW = ""
        self.keyPEM = self.generatePrivateKey()
        self.csrRAW = ""
        self.csrPEM = self.generateCSR()
        self.crtRAW = ""
        

        self.createEJBCAUser()
        self.signCert()
        self.crtPEM = self.saveCRT()

    def saveCRT(self):
        return ("-----BEGIN CERTIFICATE-----\n"
                       + self.crtRAW
                       + "\n-----END CERTIFICATE-----\n")

    def generatePrivateKey(self):
        bitLen = 2048
        key = crypto.PKey()
        key.generate_key(crypto.TYPE_RSA, bitLen)
        key_file = None
        return crypto.dump_privatekey(crypto.FILETYPE_PEM, key)

    def generateCSR(self):
        # based on https://github.com/cjcotton/python-csr
        
        dnsname = conf.dns_cert
        ipaddr = []

        ss = []
        for i in dnsname:
            ss.append("DNS: %s" % i)
        for i in ipaddr:
            ss.append("IP: %s" % i)
        ss = ", ".join(ss)
        
        
        req = crypto.X509Req()
        req.get_subject().CN = self.cName
        
        # Add in extensions
        
        base_constraints = ([
        crypto.X509Extension(
                    b"keyUsage",
                    False,
                    b"Digital Signature, Non Repudiation, Key Encipherment"),
        crypto.X509Extension(b"basicConstraints", False, b"CA:FALSE"),
        ])
    
        x509_extensions = base_constraints
        
        if ss:
            san_constraint = crypto.X509Extension(b"subjectAltName", False, ss.encode("utf-8"))
            x509_extensions.append(san_constraint)

        req.add_extensions(x509_extensions)
        
        key = crypto.load_privatekey(crypto.FILETYPE_PEM, self.keyPEM)

        req.set_pubkey(key)
        req.sign(key, "sha256")

        return crypto.dump_certificate_request(crypto.FILETYPE_PEM, req).decode("ascii")


    def createEJBCAUser(self, certificateProfileName="CFREE",
                        endEntityProfileName="EMPTY_CFREE"):

        CAName = conf.ejbca_CAName
        # create the ejbca user
        req = json.dumps({
            "caName": CAName,
            "certificateProfileName": certificateProfileName,
            "clearPwd": True,
            "endEntityProfileName": endEntityProfileName,
            "keyRecoverable": False,
            "password": "dojot",
            "sendNotification": False,
            "status": 10,
            "subjectDN": "CN=" + self.cName,
            "tokenType": "USERGENERATED",
            "username": self.cName
        })

        defaultHeader = {'content-type': 'application/json',
                    'Accept': 'application/json'}

        response = requests.post(conf.ejbca_url + "/user",
                                 headers=defaultHeader,
                                 data=req)
        
        if response.status_code != 200:
            print ("Error createEJBCAUser")

    def signCert(self):
        csr = self.csrPEM
        cutDownCLR = (csr[csr.find('-----BEGIN CERTIFICATE REQUEST-----')
                  + len('-----BEGIN CERTIFICATE REQUEST-----'):
                  csr.find("-----END CERTIFICATE REQUEST-----")]
                  .replace("\n", ""))

        req = json.dumps({
            "passwd": "dojot",
            "certificate": cutDownCLR
        })

        defaultHeader = {'content-type': 'application/json',
                        'Accept': 'application/json'}
        url = conf.ejbca_url + "/sign/" + self.cName + "/pkcs10"
        response = requests.post(url,
                                 headers=defaultHeader, data=req)

        if response.status_code == 200:
            self.crtRAW = json.loads(response.content)['status']['data']
        else:
            print ("Error signCert" )
        