default: all

clean:
	find . -type f \( -name "*.snm" -o -name "*.vrb" -o -name "*.nav" -o -name "*.aux" -o -name "*.blg" -o -name "*.ptb" -o -name "*.toc" -o -name "*.bbl" -o -name "*.out" -o -name "*.log" \) | xargs --no-run-if-empty rm

slides:
	lhs2TeX slides.lhs -o slides
	pdflatex slides
	rm slides

all:
	lhs2TeX thesis.lhs -o thesis
	pdflatex thesis
	bibtex thesis
	pdflatex thesis
	pdflatex thesis

.PHONY: clean
