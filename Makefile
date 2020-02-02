MD_SOURCES=\
				docs/index.md \
				docs/introduction.md \
				docs/installation.md \
				docs/next-steps.md \
				docs/contribute.md


docs/index.html: $(MD_SOURCES) docs/template.html docs/docs.css
	pandoc $(SHARED_PANDOC_OPTIONS) \
		--to html5 \
		--standalone \
		--toc \
		--top-level-division chapter \
		"--metadata=subtitle:$(VERSION)" \
		--no-highlight \
		--css docs.css \
		--output docs/index.html \
		--shift-heading-level-by 2 \
		--template=docs/template.html \
	$(MD_SOURCES)

.PHONY: docs
docs: docs/index.html
