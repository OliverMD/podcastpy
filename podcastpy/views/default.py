import os

import feedparser
import requests
import vlc
from enum import Enum
from pyramid.response import Response, FileResponse
from pyramid.view import view_config


url = "https://rss.art19.com/nu-nl-dit-wordt-het-nieuws"


class PlayerState(Enum):
    READY = 1
    PLAYING = 2
    PAUSED = 3


class Player(object):
    def __init__(self):
        self._state = PlayerState.READY
        self._instance = vlc.Instance('-v')
        vlc.libvlc_set_log_verbosity(self._instance, vlc.LogLevel.DEBUG)
        self._vlc_player = self._instance.media_player_new()
        self.picture_url = ""
        self._feed_entry = None
        self._update_feed()

    def _update_feed(self):
        feed = feedparser.parse(url)

        self._feed_entry = feed.entries[0]
        print(self._feed_entry.published)
        print(self._feed_entry.itunes_duration)
        print(self._feed_entry.links[0].href)

        self.picture_url = self._feed_entry.image.href
        print(self.picture_url)

    def _refresh_state(self):
        if self._state == PlayerState.PLAYING or self._state == PlayerState.PAUSED:
            if self._vlc_player.get_media() is None:
                self._state = PlayerState.READY

    def get_state(self):
        return self._state

    def start_play(self):
        self._state = PlayerState.PLAYING

        self._update_feed()
        podcast_url = self._feed_entry.links[0].href
        download_file(podcast_url)
        media = self._instance.media_new("episode.mp3")
        self._vlc_player.set_media(media)
        self._vlc_player.play()

    def get_progress(self):
        return self._vlc_player.get_position()

    def toggle_pause(self):
        print(self._state)
        if self._state == PlayerState.PLAYING:
            self._state = PlayerState.PAUSED
        elif self._state == PlayerState.PAUSED:
            self._state = PlayerState.PLAYING
        self._vlc_player.pause()

    def stop(self):
        self._state = PlayerState.READY
        self._vlc_player.stop()

    def get_length(self):
        return int(self._vlc_player.get_length() / 1000)

    def get_time(self):
        return int(self._vlc_player.get_time() / 1000)


current_player = Player()


def change_volume(new_volume):
    if new_volume < 0 or new_volume > 100:
        raise ValueError("Volume must >= 0 and <= 100")
    res = os.popen('amixer -c 0 sset PCM playback {}%'.format(int(new_volume))).read()
    print(res)


def get_volume():
    res = os.popen('amixer -c 0 sget PCM playback | tail -1 | tr -d []% | awk \'{print $4}\'', 'r', 1)
    return int(res.read().strip('\n'))


def download_file(addr):
    r = requests.get(addr)
    print(r.is_redirect)
    with open("episode.mp3", "wb") as fh:
        fh.write(bytes(r.content))


@view_config(route_name='state', request_method='GET', renderer='json')
def get_state_handler(request):
    return {'progress': current_player.get_progress(),
            'length': current_player.get_length(),
            'time': current_player.get_time(),
            'paused': current_player.get_state() is PlayerState.PAUSED}


@view_config(route_name='hello', request_method='GET')
def hello_world(request):
    return Response('Hello World!')


@view_config(route_name='play', request_method='GET')
def play_handler(request):
    current_player.start_play()
    return Response('Starting...')


@view_config(route_name='volume', request_method='GET')
def get_current_volume_handler(request):
    vol = get_volume()
    print(vol)
    return Response(str(vol))


@view_config(route_name='volume', request_method='POST')
def change_volume_handler(request):
    new_vol = int(request.GET.get('vol'))
    print(new_vol)
    change_volume(new_vol)
    return Response('Volume changed')


@view_config(route_name='root')
def static_root_handler(request):
    here = os.path.dirname(__file__)
    path = os.path.join(here, '..', 'static', 'index.html')
    return FileResponse(path, request=request)


@view_config(route_name='progress', request_method='GET')
def get_progress_handler(request):
    return Response(str(current_player.get_progress()))


@view_config(route_name='pause', request_method='GET')
def toggle_pause_handler(request):
    current_player.toggle_pause()
    return Response('Toggled')


@view_config(route_name='image', request_method='GET')
def get_image_url_handler(request):
    return Response(current_player.picture_url)