import os

redis_host = os.environ.get("REDIS_HOST","172.17.0.3")
redis_port = os.environ.get("REDIS_PORT","6379")

ejbca_url = os.environ.get("EJBCA_URL","http://172.17.0.2:5583") 
ejbca_CAName = "IOTmidCA"
dns_cert = ["localhost"]

