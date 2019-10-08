import datetime
import logging
import sqlite3

from pyramid.events import subscriber, ApplicationCreated, NewRequest

from podcastpy.player.alarm_scheduler import LocalTimezone

log = logging.getLogger(__name__)


@subscriber(ApplicationCreated)
def create_db(event) -> None:
    log.info('Creating db')
    db_path = event.app.registry.settings['db']
    db = sqlite3.connect(db_path)
    db.execute('''create table if not exists alarms (
        id integer primary key autoincrement,
        time text,
        enabled integer
    );''')
    db.commit()
    event.app.registry.notify(DbCreated(AlarmStore(db)))
    db.close()


@subscriber(NewRequest)
def new_request_subscriber(event) -> None:
    event.request.db = AlarmStore(sqlite3.connect(event.request.registry.settings['db']))
    event.request.add_finished_callback(close_db_connection)


def close_db_connection(request) -> None:
    request.db.close()


class AlarmStore(object):
    def __init__(self, db):
        self._db = db
        self._default_time = datetime.time(hour=9, minute=30, tzinfo=LocalTimezone())

    def replace_alarm(self, time: datetime.time, enabled: bool) -> None:
        self._db.execute('delete from alarms where true;')
        self._db.execute('insert into alarms (time, enabled) values (?, ?);', (time.isoformat(), enabled))
        self._db.commit()

    def get_alarm(self) -> (datetime.time, bool):
        c = self._db.cursor()
        c.execute('select time, enabled from alarms limit 1;')
        res = c.fetchone()
        if res is None:
            self.replace_alarm(self._default_time, False)
            return self._default_time, False

        return datetime.time.fromisoformat(res[0]), res[1] > 0

    def close(self) -> None:
        self._db.close()


class DbCreated(object):
    def __init__(self, db):
        self.db = db
