#!/bin/sh

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
: '
export CERT_EJBCA_API_BROKER='172.18.0.3'
export CERT_EJBCA_API_PORT='5583'
export HOSTNAME='broker'
export BASE_DIR='/vernemq'
'
#########################################################

BASE_DIR=${BASE_DIR:-"/opt/mqtt_client"}
HOSTNAME="${HOSTNAME:-"broker"}"
CERT_CNAME="${HOSTNAME:-"broker"}"
CERT_EJBCA_API_BROKER=${CERT_EJBCA_API_BROKER:-"192.168.15.24"}
CERT_EJBCA_API_PORT=${CERT_EJBCA_API_PORT:-"5583"}

export CERT_EJBCA_URL="http://${CERT_EJBCA_API_BROKER}"
export CERT_DNS="${CERT_DNS:-"localhost"}"
export CERT_CA_FILE='ca.crt'
export CERT_CERT_FILE="$HOSTNAME.crt"
export CERT_KEY_FILE="$HOSTNAME.key"
export CERT_CSR_FILE="$HOSTNAME.csr"
export CERT_CANAME='IOTmidCA'


export certCAName=$CERT_CANAME
export certEjbcaApiUrl="${CERT_EJBCA_URL}:${CERT_EJBCA_API_PORT}"
export certCname=$CERT_CNAME
export certDns=$CERT_DNS
export certCaFile=$CERT_CA_FILE
export certCertFile=$CERT_CERT_FILE
export certKeyFile=$CERT_KEY_FILE
export certCsrFile=$CERT_CSR_FILE
export certDir="$BASE_DIR/cert"
export keyLength=2048
export password="dojot"