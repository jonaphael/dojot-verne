#!/bin/bash

#########################################################
#########################################################

certCAName="IOTmidCA"
certEjbcaApiUrl="http://192.168.96.1:5583"
certCaFile="cert_things/ca.crt"

_saveFormattedCRT()
{
  nameFile=$1
  rawCRT=$2

  echo "Saving CRT ${nameFile}"

  (echo  "-----BEGIN CERTIFICATE-----"
  echo ${rawCRT}
  echo "-----END CERTIFICATE-----"  ) > tempfile.crt

  openssl x509 -inform pem -in tempfile.crt -out ${nameFile}

  rm tempfile.crt
}

echo "Retrieve cert for  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}"
certCa=$(curl --silent -X GET ${certEjbcaApiUrl}/ca/${certCAName} \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.certificate' -r )

_saveFormattedCRT "${certCaFile}" "${certCa}"
