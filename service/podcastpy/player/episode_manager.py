import feedparser
import requests


class EpisodeManager(object):
    def __init__(self):
        self._url = "https://rss.art19.com/nu-nl-dit-wordt-het-nieuws"
        self.picture_url = ""

    def preload_episode(self):
        feed = feedparser.parse(self._url)
        feed_entry = feed.entries[0]
        self.picture_url = feed_entry.image.href
        podcast_url = feed_entry.links[0].href
        r = requests.get(podcast_url)
        with open("episode.mp3", "wb") as fh:
            fh.write(bytes(r.content))

    def get_latest_episode_path(self):
        return "episode.mp3"
