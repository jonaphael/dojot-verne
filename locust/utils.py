from locust import events

class Utils():

    @staticmethod
    def time_delta(t1, t2):
        return int((t2 - t1) * 1000)

    @staticmethod
    def fire_locust_failure(**kwargs):
        events.request_failure.fire(**kwargs)

    @staticmethod
    def fire_locust_success(**kwargs):
        events.request_success.fire(**kwargs)    