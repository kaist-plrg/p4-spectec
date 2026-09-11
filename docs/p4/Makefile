ROUGE_STYLE=github
ROUGE_CSS=style
ROUGE_LEXER=./resources/rouge/watsup.rb

SPECTEC=p4spectec
SPECFILES=../../spec/*/*.watsup
SPEC=P4-16-spec-new
RELEASE=P4-16-spec-release
DRAFT=P4-16-spec-draft

grammar.trimmed.adoc: grammar.adoc trim-asciidoc-tag-comments.py
	./trim-asciidoc-tag-comments.py $< > $@

spectec:
	cd ../.. && make release && cd docs

splice: spectec
	rm -r sections || true
	cp -r sections-skeleton sections
	SPLICEFILES=$$(ruby -rasciidoctor -e \
		'Asciidoctor.load_file(ARGV[0], safe: :unsafe, parse: true).references[:includes].each_key { |path| puts "-splice #{path}.adoc" }' \
		${SPEC}.adoc | tr '\n' ' '); \
	../../${SPECTEC} splice ${SPECFILES} -inplace $${SPLICEFILES} 2> splice.missing
	python3 text_missing.py > text.missing

${DRAFT}.html: ${SPEC}.adoc splice grammar.adoc grammar.trimmed.adoc
	time asciidoctor -q \
		-r $(ROUGE_LEXER) \
		-a pdf-file=${DRAFT}.pdf \
		-a rouge-css=$(ROUGE_CSS) \
		-o $@ $<

${RELEASE}.html: ${SPEC}.adoc splice grammar.adoc grammar.trimmed.adoc
	time asciidoctor -q \
		-r $(ROUGE_LEXER) \
		-a release \
		-a pdf-file=${RELEASE}.pdf \
		-a rouge-css=$(ROUGE_CSS) \
		-o $@ $<

${DRAFT}.pdf: ${SPEC}.adoc splice grammar.adoc grammar.trimmed.adoc
	time asciidoctor-pdf -q \
		-r $(ROUGE_LEXER) \
		-a pdf-fontsdir=resources/fonts \
		-a rouge-style=$(ROUGE_STYLE) \
		-o $@ $<

${RELEASE}.pdf: ${SPEC}.adoc splice grammar.adoc grammar.trimmed.adoc
	time asciidoctor-pdf -q \
		-r $(ROUGE_LEXER) \
		-a release \
		-a pdf-fontsdir=resources/fonts \
		-a rouge-style=$(ROUGE_STYLE) \
		-o $@ $<

draft: ${DRAFT}.html ${DRAFT}.pdf
draft-html: ${DRAFT}.html

release: ${RELEASE}.html ${RELEASE}.pdf
release-html: ${RELEASE}.html

clean:
	/bin/rm -f ${SPEC}.pdf ${SPEC}.html ${DRAFT}.html ${DRAFT}.pdf ${RELEASE}.html ${RELEASE}.pdf grammar.trimmed.adoc resources/figs/stem-*.png
