# **Dojot VerneMQ**

The Dojot VerneMQ service is a extension of [VerneMQ](https://github.com/vernemq/vernemq) with some features for dojot case. 


## **Environment variables**

Key                      | Purpose                                                       | Default Value  | Accepted values
-----------------------  | --------------------------------------------------------------| -------------- |------------
EJBCA_HOSTNAME           | Cluster address                                               | "ejbca-wrapper"| IP or DNSs
EJBCA_PORT               | Ejbca service port                                            | "5583"         | port values
USE_VMQ_OPERATOR         | yes if use with vmq-operator                                  | "n"            | y or n
HOSTNAME                 | Name to container                                             | "broker"       | string
SERVER_HOSTNAME          | Server hostname (the host to connect)                         | "localhost"    | hostname
CHECK_EXPIRATION_TIME    | Checks if the certificates expires every define time by cron  | "0 1 * * *" | cron schedule expressions
CHECK_BROKER_CERT_REVOKED_TIME  | Checks if the public certificate of broker has revoked every define time by cron  | "0 */3 * * *" | cron schedule expressions
CRL_UPDATE_TIME          | Update CRL certificate every define time by cron              | "0 */2 * * *" | cron schedule expressions
CHECKEND_EXPIRATION_SEC  | When expiration check certificates run, renew if the certificates expires within the next arg seconds| 43200  | seconds
PLUGIN_ACL_CHAIN             | Plugin ACL - Use "y" if there's other plugin with same hook    | "n"               | y or n
PLUGIN_ACL_K2V_SERVICENAME   | service name for k2v-bridge                                    | k2v-bridge-verne  | string
PLUGIN_ACL_V2K_SERVICENAME       | service name for v2k-bridge                                    | v2k-bridge-verne  | string
PLUGIN_DISC_LIFETIME_SESSION | Plugin Disconnect -  session lifetime                          | 30 min            | integer (miliseconds)


### **VerneMQ Configuration**

In dojot case we use this configuration [see here](./vernemq.conf).

See more about verneMQ configuration in [documentation](https://docs.vernemq.com/).

### **Building Docker image with plugins**

Example:

```shell
cd plugins
./plugin_builder.sh

cd ..
docker build -t verne_img .

```