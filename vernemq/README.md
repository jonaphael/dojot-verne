# **Dojot VerneMQ**

The Dojot VerneMQ service is a extension of [VerneMQ](https://github.com/vernemq/vernemq) with some features for dojot case.  


## **Environment variables**

Key                      | Purpose                                                       | Default Value  | Accepted values
-----------------------  | --------------------------------------------------------------| -------------- |-------------------------
CERT_EJBCA_API_BROKER    | Level of debug                                                | "ejbca_simple" | IP or DNSs
CERT_EJBCA_API_PORT      | Ejbca service port                                            | "5583"         | port values
STATIC_CERT              | Use static certs                                              | "n"            | y or n 
K8S_ENV                  | K8s environment                                               | "n"            | y or n 
HOSTNAME                 | Name to container                                             | "broker"       | string
BASE_DIR                 | Path base to tls files                                        | "/vernemq"     | path
CERT_DNS                 | Server hostname                                               | "localhost"    | hostname
CHECK_EXPIRATION_TIME    | Checks if the certificates expires every define time by cron  | "*/30 * * * *" | cron schedule expressions
CRL_UPDATE_TIME          | Update CRL certificate every define time by cron               | "*/30 * * * *" | cron schedule expressions
CHECKEND_EXPIRATION_SEC  | When expiration check certificates run, renew if the certificates expires within the next arg seconds| 43200          | seconds


### **VerneMQ Configuration**

See vernemq [documentation](https://docs.vernemq.com/) or [vernemq_example.conf](vernemq_example.conf)

All configuration parameters that are available in [vernemq_example.conf](vernemq_example.conf) can be defined using the DOCKER_VERNEMQ prefix followed by the confguration parameter name. E.g: allow_anonymous=on is -e "DOCKER_VERNEMQ_ALLOW_ANONYMOUS=on" or allow_register_during_netsplit=on is -e "DOCKER_VERNEMQ_ALLOW_REGISTER_DURING_NETSPLIT=on". 

### **Building Docker image by yourself**

```shell
docker build -t verne .
docker run -it --name vernemq_run --env-file vernemq.env verne
```

Note: You can change the setting in this case at [vernemq.env](./vernemq.env)

### Find the IP of a docker container

```shell
docker inspect vernemq_run  | grep \"IPAddress\").
```
