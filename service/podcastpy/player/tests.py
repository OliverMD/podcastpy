import datetime
import logging
import unittest
from podcastpy.player.alarm_scheduler import LocalTimezone

from mock import *
import time


class AlarmTests(unittest.TestCase):
    def setUp(self) -> None:
        from podcastpy.player.alarm_scheduler import AlarmScheduler
        self.service = AlarmScheduler()
        logging.basicConfig(level=logging.DEBUG)

    def testAddOfAlarm(self):
        def mock_func():
            pass

        mock_function = create_autospec(mock_func)

        vid = self.service.add_alarm((datetime.datetime.now(tz=LocalTimezone()) + datetime.timedelta(seconds=2)).time(),
                                     mock_function)
        time.sleep(3)

        mock_function.assert_called_once()

        self.service.remove_alarm(vid)

    def testTwoAlarms(self):
        def mock1():
            pass

        def mock2():
            pass

        mock_1 = create_autospec(mock1)
        mock_2 = create_autospec(mock2)

        now = datetime.datetime.now(tz=LocalTimezone())

        vids = [self.service.add_alarm((now + datetime.timedelta(seconds=1)).time(), mock_1),
                self.service.add_alarm((now + datetime.timedelta(seconds=3)).time(), mock_2)]

        time.sleep(4)
        mock_1.assert_called_once()
        mock_2.assert_called_once()

        for vid in vids:
            self.service.remove_alarm(vid)

    def testGetNextAlarm(self):
        def mock1():
            pass

        mock_1 = create_autospec(mock1)

        now = datetime.datetime.now(tz=LocalTimezone())

        vid = self.service.add_alarm((now + datetime.timedelta(seconds=3)).time(), mock_1)

        self.assertEqual(self.service.get_next_alarm(now.time(), now.date())[1], vid)

        self.service.remove_alarm(vid)
