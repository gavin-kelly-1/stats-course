presentation=2024-babs-statistics-course
location=internal

web=/camp/stp/babs/www/html/$(location)/users/kellyg/presentations/$(presentation)
url_internal=https://bioinformatics.thecrick.org/users/kellyg/presentations/$(presentation)
url_external=https://bioinformatics.crick.ac.uk/users/kellyg/presentations/$(presentation)
url=$(url_$(location))

REVEAL=5.1.0
R=./R-4.0.3-foss-2020b

.PHONY: deploy
deploy: | dependencies
	mkdir -p $(web)
	rsync -avzp index.html $(web)/
	rsync -avzp htaccess $(web)/.htaccess
	rsync -avzp images $(web)/
	rsync -avzp r-images $(web)/
	[ -d d3-fig/plotly ] || mv plotly d3-fig/
	rsync -avzp d3-fig $(web)/
	sed '/<!-- excl1  -->/,/<!-- incl1  -->/d' $(web)/index.html > $(web)/week1.html
	sed -i 's/class="poll /class="/'  $(web)/week1.html
	sed '/<!-- excl2  -->/,/<!-- incl2  -->/d' $(web)/index.html > $(web)/week2.html
	sed -i 's/class="poll /class="/'  $(web)/week2.html

dependencies: $(web)/poll/api/poll.db

$(web)/poll/api/poll.db:
	rsync -avzp jquery $(web)/
	rsync -avzp reveal $(web)/
	rsync -avzp poll $(web)/
	sed  's,var url =.*,var url = "$(url)/poll",' $(web)/poll/poll.js > $(web)/reveal/plugin/poll.js
	chmod 777 $(web)/poll/api
	chmod 777 $(web)/poll/api/poll.db


build: r-image/quiz_qr.png

.PHONY: r-image/quiz_qr.png
r-image/quiz_qr.png:
	$(R) --no-save --args $(url)/poll/ < qr.r

clean:
	rm -rf $(web)
	rm -rf shortcuts

downloads:
	mkdir -p images

.PHONY: reveal
reveal:
	wget https://github.com/hakimel/reveal.js/archive/refs/tags/$(REVEAL).tar.gz
	tar -xzf $(REVEAL).tar.gz && rm -f $(REVEAL).tar.gz
	mkdir -p $@
	rm -rf $@/dist
	mv reveal.js-$(REVEAL)/dist $@/
	mv reveal.js-$(REVEAL)/LICENSE $@/
	rm -rf  reveal.js-$(REVEAL)

jquery:
	mkdir $@
	cd $@; wget https://code.jquery.com/jquery-3.6.1.min.js



publish:
	mkdir -p shortcuts
	echo "<!doctype html>" > shortcuts/$(location).html
	echo "<script>" >> shortcuts/$(location).html
	echo "window.location.replace('$(url)')" >> shortcuts/$(location).html
	echo "</script>"  >> shortcuts/$(location).html
	ln -sfn $(web) shortcuts/$(location)
