#!/bin/bash

_saveFormattedCRT()
{
  nameFile=$1
  rawCRT=$2

  echo "Saving CRT ${nameFile}"

  (echo  "-----BEGIN CERTIFICATE-----"
  echo ${rawCRT}
  echo "-----END CERTIFICATE-----"  ) > tempfile.crt

  openssl x509 -inform pem -in tempfile.crt -out ${nameFile}

}
