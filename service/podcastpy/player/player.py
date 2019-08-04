import logging

import vlc
from enum import Enum


class PlayerState(Enum):
    NotPlaying = 1
    Starting = 2
    Playing = 3
    Paused = 4
    Stopping = 5


class Player(object):
    def __init__(self):
        self._vlc_instance = vlc.Instance('-v')
        self._vlc_player = self._vlc_instance.media_player_new()

        self._vlc_player.audio_set_volume(40)  # Set default volume

        self._state = PlayerState.NotPlaying
        self._log = logging.getLogger(__name__)

    def play(self, file_path):
        # We don't care about state, just start playing
        self._state = PlayerState.Playing
        media = self._vlc_instance.media_new(file_path)
        self._vlc_player.set_media(media)
        self._vlc_player.play()

    def pause(self):
        if self._state == PlayerState.Playing:
            self._state = PlayerState.Paused
            self._vlc_player.pause()
        else:
            self._log.error("Illegal pause called when in state: ", self._state)

    def unpause(self):
        if self._state == PlayerState.Paused:
            self._state = PlayerState.Playing
            self._vlc_player.pause()
        else:
            self._log.error("Illegal unpause called when in state: ", self._state)

    def stop(self):
        if self._state != PlayerState.NotPlaying:
            self._state = PlayerState.NotPlaying
            self._vlc_player.stop()
        else:
            self._log.error("Illegal stop called when in state: ", self._state)

    def skip(self):
        raise NotImplementedError

    def set_progress(self):
        raise NotImplementedError

    def get_progress(self):
        if self._state in [PlayerState.Playing, PlayerState.Paused]:
            return self._vlc_player.get_position()
        else:
            return 0

    def set_volume(self, volume):
        if volume < 0 or volume > 100:
            raise ValueError("Volume must >= 0 and <= 100")
        return self._vlc_player.audio_set_volume(volume)

    def get_volume(self):
        return int(self._vlc_player.audio_get_volume())

    def get_media_length(self):
        return int(self._vlc_player.get_length() / 1000)

    def get_time(self):
        return int(self._vlc_player.get_time() / 1000)

    def is_paused(self):
        return self._state is PlayerState.Paused

    def get_state(self):
        return self._state
