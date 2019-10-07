#!/usr/bin/python

import zeep
from zeep.transports import Transport

import OpenSSL.crypto
import stat

import os
import requests

ejbcaWSDLbase = None


def ejbcaServ():
    global ejbcaWSDLbase
    return ejbcaWSDLbase.service


def pfx_to_pem(pfx_path, pfx_password):
    # based on
    # https://gist.github.com/erikbern/756b1d8df2d1487497d29b90e81f8068
    ''' Decrypts the .pfx or .p12 file to be used with requests. '''
    f_pem = open('/opt/p12/superadmin.pem', 'wb')
    pfx = open(pfx_path, 'rb').read()
    p12 = OpenSSL.crypto.load_pkcs12(pfx, pfx_password)
    f_pem.write(OpenSSL.crypto.dump_privatekey(
        OpenSSL.crypto.FILETYPE_PEM, p12.get_privatekey()))
    f_pem.write(OpenSSL.crypto.dump_certificate(
        OpenSSL.crypto.FILETYPE_PEM, p12.get_certificate()))
    ca = p12.get_ca_certificates()
    if ca is not None:
        for cert in ca:
            f_pem.write(OpenSSL.crypto.dump_certificate(
                OpenSSL.crypto.FILETYPE_PEM, cert))
    f_pem.close()
    os.chmod('/opt/p12/superadmin.pem', stat.S_IRUSR)


def createPEMfromP12():
        if not os.path.isfile('/opt/p12/superadmin.pem'):
            if not os.path.isfile('/opt/p12/soap_client.p12'):
                print("Client certificate 'superadmin.p12' not found")
                return False
        pfx_to_pem('/opt/p12/soap_client.p12', 'secret')
        return True


def retrieveCACert():
    try:
        cert = ejbcaServ().getLastCAChain('IOTmidCA')[0]['certificateData']
    except zeep.exceptions.Fault as error:
        print('Error occurred while loading CA cert chain. soap message: ' + error.message)
        exit(-1)
    certStr = str(cert, 'utf-8')
    caCrt = ("-----BEGIN CERTIFICATE-----\n"
                + certStr + "\n-----END CERTIFICATE-----\n")

    with open('/opt/p12/ca.crt', "w") as crtFile:
        crtFile.write(caCrt)


def loadWSDLbase():
    global ejbcaWSDLbase

    session = requests.Session()
    if not os.path.isfile('/opt/p12/ca.crt'):
        session.verify = False
    else:
        session.verify = '/opt/p12/ca.crt'

    session.cert = '/opt/p12/superadmin.pem'
    transport = Transport(session=session)
    ejbcaWSDLbase = zeep.Client(
        'https://localhost:8443/ejbca/ejbcaws/ejbcaws?wsdl', transport=transport)

    ejbcaWSDLbase.settings(raw_response=True)


def initConf():

    if createPEMfromP12() is True:
        # Load the SOAP client without credentials to get ca.crt
        loadWSDLbase()

        # Retrieve the cert using SOAP client
        retrieveCACert()

        # Reload the SOAP client with credentials for full use
        # loadWSDLbase()

    else:
        print("Error loading the certificate")
