MD_SOURCES=\
				docs/index.md \
				docs/introduction.md \
				docs/installation.md \
				docs/next-steps.md \
				docs/contribute.md


docs/index.html: $(MD_SOURCES) docs/template.html docs/docs.css
	pandoc $(SHARED_PANDOC_OPTIONS) \
		-t html5 \
		--standalone \
		-S \
		--toc \
		--chapters \
		"--metadata=subtitle:$(VERSION)" \
		--no-highlight \
		-c docs.css \
		-o docs/index.html \
		--base-header-level=2 \
		--template=docs/template.html \
	$(MD_SOURCES)

.PHONY: docs
docs: docs/index.html
