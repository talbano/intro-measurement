# Intro to Educational and Psychological Measurement: in R

This is the initial text and code behind the book in progress titled *Introduction to Educational and Psychological Measurement: in R*

The book is built using [knitr](http://yihui.name/knitr/) and [bookdown](https://github.com/rstudio/bookdown). Contribute and submit requests at [https://github.com/talbano/intro-measurement](https://github.com/talbano/intro-measurement).

## Notes on rendering

As of 041516, numbered equations aren't compiling correctly. I started with $$\begin{equation} \end{equation}$$, which works for HTML, but produces an error in Latex. Removing the $$, as recommended in the bookdown documentation, drops the equations entirely from HTML. May need to get latest dev version of the package.

Also, specifying bookdown::pdf_book as output, with arguments, doesn't produce a pdf when knitting the index.Rmd. If I run bookdown::pdf_book() directly on the file, it works. The book document class is bad. Also, chapters are coded as sections, and section numbering is strange.
