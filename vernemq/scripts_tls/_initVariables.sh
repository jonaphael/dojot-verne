#!/bin/sh

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
: '
export CERT_EJBCA_API_BROKER='localhost'
export CERT_EJBCA_API_PORT='5583'
export STATIC_CERT='n'
export K8S_ENV='n'
export HOSTNAME='broker'
export CRL_UPDATE_TIME='*/30 * * * *'
export BASE_DIR='/vernemq'
export CHECKEND_EXPIRATION_SEC='43200'
export CHECK_EXPIRATION_TIME='*/30 * * * *'
export CHECK_BROKER_CERT_REVOKED_TIME='*/30 * * * *'
'
#########################################################

BASE_DIR=${BASE_DIR:-"/vernemq"}
HOSTNAME="${HOSTNAME:-"broker"}"
CERT_CNAME="${HOSTNAME:-"broker"}"
CERT_EJBCA_API_BROKER=${CERT_EJBCA_API_BROKER:-"localhost"}
CERT_EJBCA_API_PORT=${CERT_EJBCA_API_PORT:-"5583"}

export CHECKEND_EXPIRATION_SEC="${CHECKEND_EXPIRATION_SEC:-43200}" #12h
export CERT_EJBCA_URL="http://${CERT_EJBCA_API_BROKER}"
export CERT_DNS="${CERT_DNS:-"localhost"}"
export CERT_CA_FILE='ca.crt'
export CERT_CRL_FILE='ca.crl'
export CERT_CERT_FILE="$HOSTNAME.crt"
export CERT_KEY_FILE="$HOSTNAME.key"
export CERT_CSR_FILE="$HOSTNAME.csr"
export CERT_CANAME='IOTmidCA'
#Read up on cron patterns here (http://crontab.org/)
#By default will be updated every 2 hours
export CRL_UPDATE_TIME="${CRL_UPDATE_TIME:-"0 */2 * * *"}"
#By default will be updated every day at 1 am
export CHECK_EXPIRATION_TIME="${CHECK_EXPIRATION_TIME:-"0 1 * * *"}"
#By default will be updated every 3 hours
export CHECK_BROKER_CERT_REVOKED_TIME="${CHECK_BROKER_CERT_REVOKED_TIME:-"0 */3 * * *"}"

export IS_K8S_ENV=${K8S_ENV:-"n"}
export certCAName=$CERT_CANAME
export certEjbcaApiUrl="${CERT_EJBCA_URL}:${CERT_EJBCA_API_PORT}"
export certCname=$CERT_CNAME
export certDns=$CERT_DNS
export certCaFile=$CERT_CA_FILE
export certCertFile=$CERT_CERT_FILE
export certKeyFile=$CERT_KEY_FILE
export certCsrFile=$CERT_CSR_FILE
export certCrlFile=$CERT_CRL_FILE
export certDir="$BASE_DIR/cert"
export USE_STATIC_CERTS=${STATIC_CERT:-"n"}
export keyLength=2048
export password="dojot"

