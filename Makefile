
.PHONY: clean build

build: ui/src/Main.elm
	mkdir -p build/podcastpy
	cp -r service/podcastpy/* build/podcastpy/
	cp service/production.ini build/
	cp service/setup.py build/
	cp service/README.txt build/
	cp service/MANIFEST.in build/
	cp service/CHANGES.txt build/
	cd service && pipenv lock -r > ../build/requirements.txt
	cd ui && npm run prod --nodebug

clean:
	rm -rf build