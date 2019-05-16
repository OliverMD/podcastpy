
.PHONY: clean build

build: src/Main.elm
	mkdir -p build/podcastpy
	cp -r podcastpy/* build/podcastpy/
	cp production.ini build/
	cp setup.py build/
	cp README.txt build/
	cp MANIFEST.in build/
	cp CHANGES.txt build/
	elm make src/Main.elm --optimize --output build/podcastpy/static/index.html
	pipenv lock -r > build/requirements.txt

clean:
	rm -rf build