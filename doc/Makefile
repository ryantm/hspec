jekyll=bundle exec jekyll

all:
	$(jekyll)

watch:
	$(jekyll) --auto


# only generate css + copy js
less:
	lessc css/custom.less > _site/css/custom.css
	cp js/*.js _site/js

watch-less: less
	watchr -e 'watch("(^js/.*\.js)|(^css/.*\.less)") {system "make less"};'

setup: pygments hspec
	bundle install

pygments:
	git clone https://github.com/sol/pygments -b hspec-doc

hspec:
	cd .. && \
		cabal sandbox init && \
		cabal install --only-dependencies && \
		cabal build

clean:
	-rm -r _site
	-rm -r .cache
