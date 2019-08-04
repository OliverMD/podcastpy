import datetime
import os

from pyramid.response import Response, FileResponse
from pyramid.view import view_config

from podcastpy.player.alarm_controller import get_default_alarm_controller
from podcastpy.player.alarm_scheduler import LocalTimezone

alarm_controller = get_default_alarm_controller()


@view_config(route_name='state', request_method='GET', renderer='json')
def get_state_handler(request):
    return {'progress': alarm_controller.get_player().get_progress(),
            'length': alarm_controller.get_player().get_media_length(),
            'time': alarm_controller.get_player().get_time(),
            'paused': alarm_controller.get_player().is_paused()}


@view_config(route_name='hello', request_method='GET')
def hello_world(request):
    return Response('Hello World!')


@view_config(route_name='play', request_method='GET')
def play_handler(request):
    alarm_controller.play_episode()
    return Response('Starting...')


@view_config(route_name='volume', request_method='GET')
def get_current_volume_handler(request):
    vol = alarm_controller.get_player().get_volume()
    return Response(str(vol))


@view_config(route_name='volume', request_method='POST')
def change_volume_handler(request):
    new_vol = int(request.GET.get('vol'))
    alarm_controller.get_player().set_volume(new_vol)
    return Response('Volume changed')


@view_config(route_name='root')
def static_root_handler(request):
    here = os.path.dirname(__file__)
    path = os.path.join(here, '..', 'static', 'index.html')
    return FileResponse(path, request=request)


@view_config(route_name='progress', request_method='GET')
def get_progress_handler(request):
    return Response(str(alarm_controller.get_player().get_progress()))


@view_config(route_name='pause', request_method='GET')
def toggle_pause_handler(request):
    if alarm_controller.get_player().is_paused():
        alarm_controller.get_player().unpause()
    else:
        alarm_controller.get_player().pause()
    return Response('Toggled')


@view_config(route_name='image', request_method='GET')
def get_image_url_handler(request):
    return Response(alarm_controller.get_image_url())


@view_config(route_name='alarm', request_method='GET', renderer='json')
def get_next_alarm_time(request):
    next_time = alarm_controller.get_next_alarm_time()
    return {'hour': next_time.hour, 'minute': next_time.minute}


@view_config(route_name='alarm', request_method='POST')
def change_alarm_time(request):
    new_time = datetime.datetime.combine(
        datetime.datetime.today(),
        datetime.time(hour=request.json['hour'],
                      minute=request.json['minute'],
                      tzinfo=LocalTimezone()))
    alarm_controller.change_alarm_time(new_time)
    return Response('Success')


@view_config(route_name='alarm', request_method='OPTIONS')
def handle_options(request):
    return Response()
