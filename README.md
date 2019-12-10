
## Overview

### Iot-Agent MQTT for Dojot

The Dojot VerneMQ service is a extension of [VerneMQ](https://github.com/vernemq/vernemq) with some features for dojot case. 

![cached image](http://www.plantuml.com/plantuml/proxy?src=https://raw.githubusercontent.com/mprevide/dojot-verne/doc_sol/docs/plant_uml/mqtt/diag_mqtt_)

Fig. 1 - VerneMQ with Dojot

## V2K-bridge

The V2K-bridge service is the core between the communication of Vernemq broker and the Kafka broker.

For more see [here](./mqtt-bridge)

## K2V-bridge

The K2V-bridge service is who consume messages from kafka.

For more see [here](./mqtt-bridge)


## MQTT Security: VerneMQ + TLS + PKI (EJBCA) + ACL 

We use **TLS** mutual authentication to provide a secure communication between things and broker ([VerneMQ](https://github.com/vernemq/vernemq)) through **MQTT**.  Transport Layer Security (**TLS**) is a cryptographic protocol designed to provide communications security over a computer network. 

In **TLS** each thing and broker should be provisioned with a private key and corresponding public certificate sign from **CA** (certificate authority) and a root certificate (public certificate of **CA**), the **CA** is include in **PKI** (public key infrastructure). A **PKI**  ([EJBCA](./ejbca)) is a system for the creation, storage, and distribution of digital certificates.

An **ACL** (access-control list) based authorization is provided to manage permissions, a thing can publish and subscription in own topic, the topics are: 

- For publish: ***tenant***_:_***thing_id***_/attrs_
- For subscription: ***tenant***_:_***thing_id***_/config_

Where ***tenant*** is the information context separation of dojot and ***thing_id*** is a unique identification for the thing.  
The junction (***tenant:thing_id***) of *tenant* and  *thing_id* must be unique.

Also a PKI includes the certificate revocation list (**CRL**), which is a list of certificates that have been revoked before reaching the expiration date of the certificate.

The process of obtaining certificates for client (Fig. 2):

 - Creation of entity at EJBCA, usually this unique entity consisting of ***tenant***_:_***thing_id**.


- Obtaining a public certificate for the client, which happens with the creation of the CSR based on the client key pair and  sending this CSR to obtain a public certificate.

- Obtaining the root certificate (public certificate of **CA**).


![cached image](http://www.plantuml.com/plantuml/proxy?src=https://raw.githubusercontent.com/mprevide/dojot-verne/doc_sol/docs/plant_uml/mqtt/seq_sec_client)

Fig. 2 - Client retrive certificates from PKI (EJBCA)


The process of obtaining certificates for VerneMQ instances (broker) follows the same steps as for the client, with some more (Fig. 3):

- At each defined time (CHECK_EXPIRATION_TIME), it's checked if the root certificate and public certificate of the VerneMQ instance will expire in the next CHECKEND_EXPIRATION_SEC seconds.

- CRL updated the CRL certificate every time by setting in CRL_UPDATE_TIME

![cached image](http://www.plantuml.com/plantuml/proxy?src=https://raw.githubusercontent.com/mprevide/dojot-verne/doc_sol/docs/plant_uml/mqtt/seq_sec_verne) 

Fig. 3 - VerneMQ (Broker) retrive certificates from PKI (EJBCA)

Environment variables mentioned above are more described in [here](./vernemq)