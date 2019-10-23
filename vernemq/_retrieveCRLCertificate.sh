#!/usr/bin/env bash

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
: '
export CERT_EJBCA_API_BROKER = 'ejbca_simple'
export CERT_EJBCA_URL= '5583'
export HOSTNAME = 'vernemq-k8s-0'
'
#########################################################

CERT_CNAME="${HOSTNAME:-"vernemq"}"
CERT_EJBCA_API_BROKER=${CERT_EJBCA_API_BROKER:-"ejbca_simple"}
CERT_EJBCA_API_PORT=${CERT_EJBCA_API_PORT:-"5583"}
CERT_EJBCA_URL="http://${CERT_EJBCA_API_BROKER}"
CERT_CRL_FILE='ca.crl'

certCAName=$CERT_CANAME
certEjbcaApiUrl="${CERT_EJBCA_URL}:${CERT_EJBCA_API_PORT}"
certCrlFile=$CERT_CRL_FILE

echo "Retrieve crl from  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}/crl"
certCrl=$(curl --silent -X GET ${certEjbcaApiUrl}/ca/${certCAName}/crl \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.CRL' -r)

(echo  "-----BEGIN X509 CRL-----"
echo ${certCrl}
echo "-----END X509 CRL-----"  ) > tempcrl.crl

openssl crl -inform pem -in tempcrl.crl -out ${certCrlFile}
