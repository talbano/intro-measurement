gitbook:
	Rscript --quiet R/_render.R "bookdown::gitbook"

all:
	Rscript --quiet R/_render.R
