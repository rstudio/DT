
HTML_FILES := $(patsubst %.Rmd, %.html ,$(wildcard *.Rmd)) \
              $(patsubst %.md, %.html ,$(wildcard *.md))

all: html


html: $(HTML_FILES)

%.html: %.Rmd
	Rscript compile.R "$<"

%.html: %.md
	Rscript compile.R "$<"

site:
	Rscript site.R

.PHONY: clean
clean:
	$(RM) -r libs $(HTML_FILES)

