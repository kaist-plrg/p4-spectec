ROUGE_STYLE=github
ROUGE_CSS=style

SPECTEC=p4spectec
SPECFILES=../../spec-meta/sl
SPEC=SL-spec-skel
GEN=SL-spec

spectec:
	cd ../.. && make release && cd docs

splice: spectec
	rm -r sections || true
	cp -r sections-skeleton sections
	SPLICEFILES=$$(find sections -type f -name '*.adoc' | xargs -I {} echo "-splice {}" | tr '\n' ' '); \
	../../${SPECTEC} splice ${SPECFILES} -inplace $${SPLICEFILES} 2> splice.missing

${GEN}.html: ${SPEC}.adoc splice
	time asciidoctor -q \
		-a rouge-css=$(ROUGE_CSS) \
		-o $@ $<

${GEN}.pdf: ${SPEC}.adoc splice
	time asciidoctor-pdf -q \
		-a pdf-fontsdir=resources/fonts \
		-a rouge-style=$(ROUGE_STYLE) \
		-o $@ $<

spec: ${GEN}.html ${GEN}.pdf
spec-html: ${GEN}.html

release: ${RELEASE}.html ${RELEASE}.pdf
release-html: ${RELEASE}.html

clean:
	/bin/rm -f ${SPEC}.pdf ${SPEC}.html ${GEN}.html ${GEN}.pdf
