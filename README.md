# Podcastpy

A podcast player for a raspberry pi. Frontend written with Elm, backend in Python 3. I use this along with cron to function as a podcast alarm clock.

The interface has been designed for use in a phone's web browser.

![Example interface](example.jpg)
## Building
`make build`: Builds the frontend & copies the backend to `./build` along with a `requirements.txt`

## Future work
* Allow the podcast played to be configurable
    * Requires adding another page to the frontend
    * Requires work on the backend
* Improve deployment process
* Allow scheduling of alarm rather than relying on Cron
* Make the frontend reactive so it makes better use of screen space on non-phones