import datetime
import logging

from podcastpy.player.alarm_scheduler import AlarmScheduler, LocalTimezone
from podcastpy.player.alarm_store import AlarmStore
from podcastpy.player.episode_manager import EpisodeManager
from podcastpy.player.player import Player, PlayerState


def get_default_alarm_controller(db):
    return AlarmController(AlarmScheduler(), EpisodeManager(), Player(), db)


class AlarmController(object):
    def __init__(self, scheduler, manager, player, db: AlarmStore):
        self._scheduler = scheduler
        self._manager = manager
        self._player = player
        self._log = logging.getLogger(__name__)

        self._log.info("Creating new AlarmController")

        alarm, enabled = db.get_alarm()
        default_time = datetime.datetime.combine(datetime.datetime.today(), alarm)

        self._alarm_enabled = enabled
        self._alarm_vid = self._scheduler.add_alarm(default_time.time(), self.play_episode)
        self._preload_vid = self._scheduler.add_alarm((default_time - datetime.timedelta(seconds=60)).time(), self.download_episode)
        self._log.info("Initial alarm id: {}".format(self._alarm_vid))
        self._log.info("Initial preload id: {}".format(self._preload_vid))

        self._manager.preload_episode()

    def change_alarm_time(self, new_time: datetime.datetime, enabled: bool, db: AlarmStore) -> None:
        if self._alarm_vid is None or self._preload_vid is None:
            raise RuntimeError("No alarm set")

        if self._scheduler.remove_alarm(self._alarm_vid) is False:
            raise RuntimeError("Could not find existing alarm task")

        if self._scheduler.remove_alarm(self._preload_vid) is False:
            raise RuntimeError("Could not find existing preload task")

        self._alarm_enabled = enabled
        db.replace_alarm(new_time.time(), enabled)
        self._preload_vid = self._scheduler.add_alarm((new_time - datetime.timedelta(seconds=60)).time(), self.download_episode)
        self._alarm_vid = self._scheduler.add_alarm(new_time.time(), self.play_episode)

    def get_next_alarm_time(self) -> (datetime.datetime, bool):
        return self._scheduler.get_alarm_time(self._alarm_vid), self._alarm_enabled

    def download_episode(self):
        self._manager.preload_episode()

    def play_episode(self):
        if not self._alarm_enabled:
            return

        # This kinda breaks abstraction, should have a hard stop. Should not care about internal state.
        if self._player.get_state() is not PlayerState.NotPlaying:
            self._player.stop()

        self._player.play(self._manager.get_latest_episode_path())

    def get_player(self):
        return self._player

    def get_image_url(self):
        return self._manager.picture_url
