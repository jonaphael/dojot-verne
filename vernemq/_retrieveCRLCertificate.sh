#!/bin/bash

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
: '
export CERT_EJBCA_API_BROKER='ejbca_simple'
export CERT_EJBCA_URL='5583'
export HOSTNAME='vernemq-k8s-0'
'
#########################################################

#!/bin/sh

CERT_EJBCA_API_BROKER="${CERT_EJBCA_API_BROKER:-"ejbca_simple"}"
CERT_EJBCA_API_PORT="${CERT_EJBCA_API_PORT:-"5583"}"
CERT_EJBCA_URL="http://${CERT_EJBCA_API_BROKER}"
CERT_CRL_FILE='/vernemq/cert/ca.crl'
CERT_CANAME='IOTmidCA'

certCAName=${CERT_CANAME}
certEjbcaApiUrl="${CERT_EJBCA_URL}:${CERT_EJBCA_API_PORT}"
certCrlFile=${CERT_CRL_FILE}

echo "Retrieve crl from  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}/crl?update=true"
certCrl=$(curl --silent -X GET ${certEjbcaApiUrl}/ca/${certCAName}/crl?update=true \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.CRL' -r)

(echo  "-----BEGIN X509 CRL-----"
echo ${certCrl}
echo "-----END X509 CRL-----"  ) > /vernemq/cert/tempcrl.crl

openssl crl -inform pem -in /vernemq/cert/tempcrl.crl -out ${certCrlFile}

chmod +x ${certCrlFile}

rm  /vernemq/cert/tempcrl.crl


