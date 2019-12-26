"""
Log-related module.
"""
from datetime import datetime
import os

from src.config import CONFIG


class LogController:
    """
    Constrols the writing in log files.
    """

    def __init__(self, run_id: str):
        self.do_log = CONFIG['app']['log_in_file']

        if self.do_log:
            self.messages = list()

            # The directory named 'run_id' will be used to store
            # all files related to the client's run
            self.client_dir = "{0}/{1}/".format(
                CONFIG['locust']['log_dir'], run_id)
            # Since the UUID is unique, we do not check whether the directory exists or not
            os.makedirs(self.client_dir)

    def save_log_list(self) -> None:
        """
        Saves the list of log messages in the log file.
        """

        if self.do_log:
            with open(self.client_dir + str(datetime.timestamp(datetime.now())) + ".log", "w") \
                    as log_file:
                log_file.writelines(self.messages)

            self.messages.clear()

    def append_log(self, message: str) -> None:
        """
        Appends a message in the log list.
        """
        if self.do_log:
            if (len(self.messages) >= CONFIG['locust']['log_message_limit']) and \
                    (len(self.messages) > 0):
                self.save_log_list()

            self.messages.append(message)
