#!/bin/bash

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
: '
export CERT_EJBCA_API_BROKER='ejbca_simple'
export CERT_EJBCA_URL='5583'
export HOSTNAME='broker'
export BASE_DIR='/vernemq'
'
#########################################################
BASE_DIR=${BASE_DIR:-"/vernemq"}

. ${BASE_DIR}/scripts_tls/_initVariables.sh

. ${BASE_DIR}/scripts_tls/saveFormattedCRT.sh

echo "Retrieve cert for  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}"
certCa=$(curl --silent -X GET ${certEjbcaApiUrl}/ca/${certCAName} \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.certificate' -r )

_saveFormattedCRT "${certDir}/${certCaFile}" "${certCa}"
