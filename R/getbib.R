readbibkey <- function(x = ".", ext, subset, recursive = FALSE,
  ignore.case = TRUE, exclude = c("@ref")) {
  # Get all bibtex keys from a vector of text files
  # x: vector of filenames, with default being all files in current
  # working directory and subdirectories
  # ext: optional vector of filename extensions to pull from the
  # current working directory, excluding leading dot. Should be set to
  # ext = c("tex", "rnw", "rmd")
  # subset: vector of keys to search for, with keys excluded if not
  # in this vector
  # Keys are parsed from files in x based on all alpha numeric text
  # following each @ until a space, bracket, or curly bracket

  if(!missing(ext))
    ext <- paste0("\\.(", paste(ext, collapse = "|"), ")$")
  else
    ext <- NULL
  xf <- list.files(x, pattern = ext, recursive = recursive,
    ignore.case = ignore.case)
  xl <- lapply(xf, readLines)
  # Delete email addresses
  xl <- gsub("[[:alnum:]]+@[[:alnum:]]+", "", unlist(xl))
  keys <- unlist(regmatches(xl, gregexpr("@[[:alnum:]]+", xl)))
  out <- keys[!keys %in% exclude]
  out <- gsub("@", "", unique(out))
  #keys <- gsub("(.+)(@[[:alnum:]]+)(\\]|\\)|[[:blank:]])(.+)",
  #  "\\2", unlist(xl))
  #nf <- length(xl)
  #nk <- length(keys)
  return(out)
}

readbib <- function(con, keys) {
  # Get entries from bib file in connection con based on keys in keys
  
  xp <- gsub("\\,\\}", "\\}", paste(readLines(con = con),
    collapse = " "))
  xp <- gsub("\t", "", xp)
  xk <- unlist(regmatches(xp,
    gregexpr("@[[:alnum:]]+\\{[^,]+", xp)))
  xk <- gsub("@[[:alnum:]]+\\{", "", xk)
  #out <- strsplit(xp, '(?<![^\\}])\\s*(?=@)', perl = TRUE)
  out <- strsplit(xp, split = "}\\s*@")[[1]]
  out[-1] <- paste0("@", out[-1])
  out[-length(out)] <- paste0(out[-length(out)], "}")
  if(!missing(keys))
    out <- out[xk %in% keys]
  return(out)
}

writebib <- function(x, con) {
  # Currently just a wrapper for writeLines
  
  writeLines(text = x, con = con)
}

renewbib <- function(x = ".", ext, subset, recursive = FALSE,
  ignore.case = TRUE, exclude = c("@ref"), keys, bibfile, newfile) {
  # Create or overwrite a bibfile based on citations parsed from
  # files in directory x, optionally with extensions in ext
  
  if(missing(keys))
    keys <- readbibkey(x = x, ext = c("tex", "rnw", "rmd"),
      subset = subset, recursive = recursive,
      ignore.case = ignore.case, exclude = exclude)
  bib <- readbib(con = bibfile, keys = keys)
  writebib(bib, con = newfile)
}