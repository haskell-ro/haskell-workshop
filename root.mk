.PHONY: clean all slide pdf

PDF = $(addsuffix .pdf, $(BASENAME))
SLIDE = $(addsuffix .html, $(BASENAME))
TEX = $(addsuffix .tex, $(BASENAME))
MARKDOWN = $(addsuffix .md, $(BASENAME))

PANDOC = pandoc
PANDOC_HEADER = $(PANDOC) --self-contained -s
PANDOC_SLIDE = $(PANDOC_HEADER) -t dzslides
PANDOC_TEX = $(PANDOC_HEADER) -t beamer

PDFLATEX = pdflatex
OUTDIR = texfiles

all: slide pdf

slide: $(SLIDE)

pdf: $(PDF)

$(SLIDE): $(MARKDOWN)
	@$(PANDOC_SLIDE) -o $@ $<

$(TEX): $(MARKDOWN)
	@$(PANDOC_TEX) -o $@ $<

$(PDF): $(TEX)
	@test -d $(OUTDIR) || mkdir $(OUTDIR)
	@$(PDFLATEX) --output-directory $(OUTDIR) $<
	@$(PDFLATEX) --output-directory $(OUTDIR) $<
	@ln -sf $(OUTDIR)/$@ .

clean:
	@$(RM) -r $(OUTDIR)
	@$(RM) $(PDF) $(TEX) $(SLIDE) $(EXTRACLEAN)
