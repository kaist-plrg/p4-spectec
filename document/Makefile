OLDSPEC=P4-16-spec
NEWSPEC=P4-16-spec-new
ROUGE_STYLE=github
ROUGE_CSS=style

grammar.trimmed.adoc: grammar.adoc trim-asciidoc-tag-comments.py
	./trim-asciidoc-tag-comments.py $< > $@

${OLDSPEC}.pdf: ${OLDSPEC}.adoc grammar.adoc grammar.trimmed.adoc
	time asciidoctor-pdf -v \
		-r asciidoctor-mathematical \
		-a pdf-fontsdir=resources/fonts \
		-a rouge-style=$(ROUGE_STYLE) $<

${OLDSPEC}.html: ${OLDSPEC}.adoc grammar.adoc grammar.trimmed.adoc
	time asciidoctor -v \
		-r asciidoctor-mathematical \
		-a rouge-css=$(ROUGE_CSS) $<

old: ${OLDSPEC}.pdf ${OLDSPEC}.html

${NEWSPEC}.pdf: ${NEWSPEC}.adoc grammar.adoc grammar.trimmed.adoc
	time asciidoctor-pdf -v \
		-r asciidoctor-mathematical \
		-a pdf-fontsdir=resources/fonts \
		-a rouge-style=$(ROUGE_STYLE) $<

${NEWSPEC}.html: ${NEWSPEC}.adoc grammar.adoc grammar.trimmed.adoc
	time asciidoctor -v \
		-r asciidoctor-mathematical \
		-a rouge-css=$(ROUGE_CSS) $<

new: ${NEWSPEC}.pdf ${NEWSPEC}.html

clean:
	/bin/rm -f ${SPEC}.pdf ${SPEC}.html ${NEWSPEC}.pdf grammar.trimmed.adoc resources/figs/stem-*.png
