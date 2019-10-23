from datetime import datetime
from config import config


class LogController:
  """Constrols the writing in log files"""

  def __init__(self, client_dir: str):
    self.client_dir = client_dir
    self.messages = list()

  def save_log_list(self) -> None:
    """Saves the list of log messages in the log file."""

    with open(self.client_dir + str(datetime.timestamp(datetime.now())), "w") as f:
      f.writelines(self.messages)

    self.messages.clear()

  def append_log(self, message: str) -> None:
    if (len(self.messages) >= config['locust']['log_message_limit']) and (len(self.messages) > 0):
      self.save_log_list()

    self.messages.append(message)
