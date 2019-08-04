import copy
import datetime
import threading
import time as _time
import sortedcontainers
import logging

# From https://docs.python.org/3.5/library/datetime.html#datetime.tzinfo
ZERO = datetime.timedelta(0)
HOUR = datetime.timedelta(hours=1)

STDOFFSET = datetime.timedelta(seconds=-_time.timezone)
if _time.daylight:
    DSTOFFSET = datetime.timedelta(seconds=-_time.altzone)
else:
    DSTOFFSET = STDOFFSET

DSTDIFF = DSTOFFSET - STDOFFSET


class LocalTimezone(datetime.tzinfo):

    def utcoffset(self, dt):
        if dt is None:
            return STDOFFSET
        if self._isdst(dt):
            return DSTOFFSET
        else:
            return STDOFFSET

    def dst(self, dt):
        if dt is None:
            return ZERO
        if self._isdst(dt):
            return DSTDIFF
        else:
            return ZERO

    def tzname(self, dt):
        return _time.tzname[self._isdst(dt)]

    def _isdst(self, dt):
        tt = (dt.year, dt.month, dt.day,
              dt.hour, dt.minute, dt.second,
              dt.weekday(), 0, 0)
        stamp = _time.mktime(tt)
        tt = _time.localtime(stamp)
        return tt.tm_isdst > 0


class AlarmScheduler(object):
    """
    Provides an interface for scheduling daily alarms which run a given function.
    """
    def __init__(self):
        self._timer_thread = None

        # Need to be able to quickly index on time
        self._alarm_time_index = sortedcontainers.SortedDict()  # time -> alarm vId

        # Need to be able to quickly index on some id
        self._alarm_lookup = {}  # alarm vId -> (callback, alarm_time, last_trigger_date)

        self._next_vid = None
        self._log = logging.getLogger(__name__)
        self._log.info("Alarm scheduler created")

    def _reschedule(self):
        if self._timer_thread is not None:
            self._log.info("Cancelling timer thread")
            self._timer_thread.cancel()

        if len(self._alarm_lookup) is 0:
            return

        now = datetime.datetime.now(LocalTimezone())
        next_time, self._next_vid = self.get_next_alarm(now.time(), now.date())

        for_tomorrow = now.date() == self._alarm_lookup[self._next_vid][2]

        next_alarm_time = AlarmScheduler.get_next_alarm_datetime(next_time, now, for_tomorrow)
        self._log.info("Next alarm: {}".format(next_alarm_time))
        s = (next_alarm_time - now).total_seconds()
        self._timer_thread = threading.Timer(
            s, self._run_alarm, [self._alarm_lookup[self._next_vid][0], self._next_vid, next_alarm_time.date()])

        self._timer_thread.start()

    def _run_alarm(self, callback, vid, trigger_date):
        # Pass the date in as we don't want to call datetime.now() here as it might get weird with times just before
        # midnight.
        self._log.info("_run_alarm")
        self._alarm_lookup[vid] = (self._alarm_lookup[vid][0], self._alarm_lookup[vid][1], trigger_date)
        callback()
        self._reschedule()

    def add_alarm(self, time, callback_fn):
        """
        Adds an alarm to the alarm service
        :param time: The time to run the alarm at (daily)
        :param callback_fn: The function to be run upon the alarm triggering
        :return: The id of the alarm added
        """
        # Check for a time collision & apply some nudging
        self._log.info("Adding alarm at {}".format(time))
        t = copy.copy(time)
        while t in self._alarm_time_index:
            t = t + datetime.timedelta(microseconds=1)

        new_vid = hash((t, callback_fn))  # generate id using hash
        while new_vid in self._alarm_lookup:
            new_vid += 1

        self._alarm_lookup[new_vid] = (callback_fn, t, None)
        self._alarm_time_index[t] = new_vid

        now = datetime.datetime.now()
        if self.get_next_alarm(now.time(), now.date())[1] == new_vid:
            self._reschedule()

        return new_vid

    def remove_alarm(self, alarm_id):
        """
        Removes the alarm with the given alarm_id from the alarm service
        :param alarm_id: The alarm id to remove
        :return: True if alarm exists and was removed, False if alarm did not exist
        """
        self._log.info("Removing alarm: {}".format(alarm_id))
        if alarm_id not in self._alarm_lookup:
            return False

        time = self._alarm_lookup.pop(alarm_id)[1]
        idx = self._alarm_time_index.bisect_left(time)
        self._alarm_time_index.popitem(idx)

        if self._next_vid is alarm_id:
            self._reschedule()

        return True

    def get_alarm_time(self, vid):
        return self._alarm_lookup[vid][1]

    def get_next_alarm(self, at_time, now_date):
        """
        If the current time exactly matches a time in the index then the time after that is retrieved
        :return: (datetime, alarm id)
        """

        if len(self._alarm_lookup) == 0:
            raise AssertionError("No alarms")

        # We want to get the next time today that hasn't already been triggered (sometimes threading.Timer triggers
        # early). If there is only 1 alarm then we are fine to use that alarm

        idx = self._alarm_time_index.bisect_right(at_time) % len(self._alarm_time_index)
        start = idx
        while self._alarm_lookup[self._alarm_time_index.peekitem(idx)[1]][2] is now_date:
            idx = (idx + 1) % len(self._alarm_time_index)
            if start >= idx:
                break

        return self._alarm_time_index.peekitem(idx)

    @staticmethod
    def get_next_alarm_datetime(next_time, now, for_tomorrow):
        """
        Get the datetime of the next alarm
        :return: datetime.datetime
        """

        if (not for_tomorrow) and now.time() < next_time:
            return datetime.datetime.combine(now.date(), next_time.replace(tzinfo=LocalTimezone()))
        else:
            return datetime.datetime.combine(now.date() + datetime.timedelta(days=1),
                                             next_time.replace(tzinfo=LocalTimezone()))
