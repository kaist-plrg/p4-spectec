ROUGE_STYLE=github
ROUGE_CSS=style

SPECTEC=p4spectec
SPECFILES=../spec-concrete/*.watsup
SPEC=P4-16-spec-new

grammar.trimmed.adoc: grammar.adoc trim-asciidoc-tag-comments.py
	./trim-asciidoc-tag-comments.py $< > $@

spectec:
	cd .. && make build && cd document

splice: spectec
	rm -r sections || true
	cp -r sections-skeleton sections
	SPLICEFILES=$$(find sections -type f -name '*.adoc' | xargs -I {} echo "-splice {}" | tr '\n' ' '); \
	../${SPECTEC} splice ${SPECFILES} -inplace $${SPLICEFILES}

${SPEC}.pdf: ${SPEC}.adoc splice grammar.adoc grammar.trimmed.adoc
	time asciidoctor-pdf -q \
		-a pdf-fontsdir=resources/fonts \
		-a rouge-style=$(ROUGE_STYLE) $<

${SPEC}.html: ${SPEC}.adoc splice grammar.adoc grammar.trimmed.adoc
	time asciidoctor -q \
		-a rouge-css=$(ROUGE_CSS) $<

build: ${SPEC}.pdf ${SPEC}.html
build-html: ${SPEC}.html

clean:
	/bin/rm -f ${SPEC}.pdf ${SPEC}.html ${NEWSPEC}.pdf grammar.trimmed.adoc resources/figs/stem-*.png
