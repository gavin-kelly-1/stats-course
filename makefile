web=/camp/stp/babs/www/html/internal/users/kellyg/presentations/2024-babs-statistics-course

.PHONY: deploy
deploy:
	mkdir -p $(web)
	rsync -avzp index.html $(web)/
	rsync -avzp htaccess $(web)/.htaccess
	rsync -avzp images $(web)/
	rsync -avzp r-images $(web)/
	[ -d d3-fig/plotly ] || mv plotly d3-fig/
	rsync -avzp d3-fig $(web)/
	rsync -avzp reveal $(web)/
	rsync -avzp poll $(web)/
	rsync -avzp jquery $(web)/
	rsync -avzp poll $(subst internal,external,$(web))/
	sed '/<!-- excl1  -->/,/<!-- incl1  -->/d' $(web)/index.html > $(web)/week1.html
	sed -i 's/class="poll /class="/'  $(web)/week1.html
	sed '/<!-- excl2  -->/,/<!-- incl2  -->/d' $(web)/index.html > $(web)/week2.html
	sed -i 's/class="poll /class="/'  $(web)/week2.html
	sed '/<!-- excl3  -->/,/<!-- incl3  -->/d' $(web)/index.html > $(web)/week3.html
	sed -i 's/class="poll /class="/'  $(web)/week3.html
	sed '/<!-- excl4  -->/,/<!-- incl4  -->/d' $(web)/index.html > $(web)/week4.html
	sed -i 's/class="poll /class="/'  $(web)/week4.html


downloads:
	mkdir -p images

reveal:
	wget https://github.com/hakimel/reveal.js/archive/refs/tags/4.3.1.tar.gz
	tar -xzf 4.3.1.tar.gz
	mkdir -p $@
	mv reveal.js-4.3.1/dist $@/
	mv reveal.js-4.3.1/LICENSE $@/
	rm -rf 4.3.1.tar.gz reveal.js-4.3.1

jquery:
	mkdir $@
	cd $@; wget https://code.jquery.com/jquery-3.6.1.min.js
