# -------------------------------------------------------------------------------

read_block <- function(x = ".", files, ext = "Rmd", type, recursive = FALSE,
  ignore.case = TRUE, chapi = 5, ...) {
  # Get all R markdown blocks with type matching type
  # Currently assumes three or more back ticks to start a block,
  # type is listed on same line as start of block, and start of block is
  # all contained on the same line
  # x: vector of filenames, with default being all files in current
  # working directory and subdirectories
  # files: character vector of file names, in order, for chapters containing
  # blocks of interest
  # ext: optional vector of filename extensions to pull from the
  # current working directory, excluding leading dot.
  # type: character containing the block type to extract
  # recursive, ignore.case: logicals sent to list.files
  # chapi: what element in each file contains the chapter title
  
  if(!missing(ext))
    ext <- paste0("\\.(", paste(ext, collapse = "|"), ")$")
  else
    ext <- NULL
  # Get all file names
  xf <- list.files(x, pattern = ext, recursive = recursive,
    ignore.case = ignore.case)
  if (!all(files %in% xf)) stop("Some files not found")
  xl <- lapply(files, readLines)
  xu <- unlist(xl)
  # Find starting and ending indices for block chunks
  iboth <- grep("\`{3,}", xu)
  istart <- grep("\`{3,}[[:space:]]*\\{[[:space:]]*block", xu)
  istart <- istart[grep(paste0("type[[:space:]]*=[[:space:]]*\"", type), xu[istart])]
  iend <- iboth[match(istart, iboth) + 1]
  # Extract blocks
  out <- lapply(seq_along(istart), function(i) xu[istart[i]:iend[i]])
  # Get chapter names from xl
  if (length(chapi) == 1) chapi <- rep(chapi, length(xl))
  cnames <- sapply(seq_along(xl), function(i) xl[[i]][chapi[i]])
  cnames <- trim(gsub("#[[:space:]]|\\{.+\\}", "", cnames))
  # Apply names
  cnamei <- get_lindex(xl, istart)
  names(out) <- cnames[cnamei]
  return(out)
}

# -------------------------------------------------------------------------------

trim <- function(x) {
  gsub("^[[:space:]]+|[[:space:]]+$", "", x)
}

# -------------------------------------------------------------------------------

get_lindex <- function(x, y) {
  # Returns the numbers of elements in list x that contain elements of
  # the sequential index vector in y
  # Not currently used
  xlen <- sapply(x, length)
  xlenr <- c(0, sapply(seq_along(x)[-length(x)], function(i) sum(xlen[1:i])))
  xseq <- lapply(x, seq_along)
  xseqr <- sapply(seq_along(x), function(i) xseq[[i]] + xlenr[i])
  out <- sapply(y, function(z) which(unlist(lapply(xseqr, function(w) z %in% w))))
  return(out)
}

# -------------------------------------------------------------------------------
# Learning objectives

write_obj <- function(x, con, addchaps = TRUE, heading = "Learning objectives",
  chaps = paste("Chapter", 1:length(x)), titles = names(x), ...) {
  # Writes to file R markdown for learning objectives
  # x: list of code blocks extracted from files with read_block
  # con: file name to write to
  # addchaps: logical, add chapter names between lists of objectives
  # heading: heading to remove from each block
  # chaps: character vector of chapter headings to add
  # titles: character vector of chapter titles to add
  # For chapter separators, assumes that all chapters having objectives
  # also have titles and are numbered sequentially starting with 1
  clean <- function(y, delete = c("\`\`\`", "\\{.+\\}"), remove = heading) {
    for (pat in delete)
      y <- gsub(pat, "", y, ignore.case = TRUE)
    for (pat in remove)
      y <- y[!grepl(pat, y)]
    return(y)
  }
  out <- lapply(x, clean)
  names(out) <- paste0("**", paste(chaps, names(out), sep = ": "), "**")
  temp <- file(con, open = "wt")
  for (i in seq_along(out)) {
    writeLines(c(names(out)[i], "", out[[i]], ""), temp, sep = "  \n")
  }
  close(temp)
}

renew_obj <- function(x = ".", files, type = "objectives",
  newfile = "objectives.Rmd", ...) {
  # Create or overwrite a learning objectives file
  out <- read_block(x = x, files = files, type = type, ...)
  write_obj(out, con = newfile, ...)
}

# -------------------------------------------------------------------------------
# Learning checks

# Not done with this...
# chaps isn't going to work
write_check <- function(x, con, addchaps = TRUE,
  delete = c("\`\`\`", "\\{.+\\}", "\\*\\*Learning check\\*\\*: "),
  chaps = paste("Chapter", 1:length(x)), titles = names(x), ...) {
  # Writes to file R markdown for learning objectives
  # x: list of code blocks extracted from files with read_block
  # con: file name to write to
  # addchaps: logical, add chapter names between lists of objectives
  # heading: heading to remove from each block
  # chaps: character vector of chapter headings to add
  # titles: character vector of chapter titles to add
  # For chapter separators, assumes that all chapters having objectives
  # also have titles and are numbered sequentially starting with 1
  clean <- function(y) {
    for (pat in c("\`\`\`", "\\{.+\\}", "\\*\\*Learning check\\*\\*: "))
      y <- gsub(pat, "", y, ignore.case = TRUE)
    return(y)
  }
  out <- lapply(x, clean)
  names(out) <- paste0("**", paste(chaps, names(out), sep = ": "), "**")
  temp <- file(con, open = "wt")
  for (i in seq_along(out)) {
    writeLines(c(names(out)[i], "", out[[i]], ""), temp, sep = "  \n")
  }
  close(temp)
}
#renew_check <- function()

# -------------------------------------------------------------------------------
# R code

read_code <- function(x = ".", files, ext = "Rmd", recursive = FALSE,
  chaps, ignore.case = TRUE, chapi = 5, ...) {
  # Get all R markdown blocks with type matching type
  if(!missing(ext))
    ext <- paste0("\\.(", paste(ext, collapse = "|"), ")$")
  else
    ext <- NULL
  # Get all file names
  xf <- list.files(x, pattern = ext, recursive = recursive,
    ignore.case = ignore.case)
  if (!all(files %in% xf)) stop("Some files not found")
  xl <- lapply(files, readLines)
  xu <- unlist(xl)
  # Find starting and ending indices for code chunks
  iboth <- grep("\`{3,}", xu)
  istart <- grep("\`{3,}[[:space:]]*\\{[[:space:]]*r", xu)
  istart <- istart[!grepl("eval[[:space:]]*=[[:space:]]*FALSE", xu[istart])]
  istart <- istart[!grepl("echo[[:space:]]*=[[:space:]]*FALSE", xu[istart])]
  iend <- iboth[match(istart, iboth) + 1]
  # Extract r code
  out <- lapply(seq_along(istart), function(i) xu[istart[i]:iend[i]])
  # Get chapter names from xl
  if (length(chapi) == 1) chapi <- rep(chapi, length(xl))
  cnames <- sapply(seq_along(xl), function(i) xl[[i]][chapi[i]])
  cnames <- trim(gsub("#[[:space:]]|\\{.+\\}", "", cnames))
  # Apply names
  cnamei <- get_lindex(xl, istart)
  if (missing(chaps)) chaps <- paste("Chapter", 1:length(out))
  names(out) <- paste0("**", paste(chaps[cnamei], cnames[cnamei], sep = ": "), "**")
  return(out)
}
write_code <- function(x, con, addchaps = TRUE, ...) {
  clean <- function(y) {
    for (pat in c("\`\`\`", "\\{.+\\}"))
      y <- gsub(pat, "", y, ignore.case = TRUE)
    return(y)
  }
  out <- lapply(x, clean)
  lnames <- unique(names(out))
  tempf <- file(con, open = "wt")
  for (i in lnames) {
    tempt <- c(i, "", unlist(out[names(out) == i]), "")
    names(tempt) <- NULL
    writeLines(, tempf)
  }
  close(temp)
}
renew_code <- function(x, files, newfile) {
  knitr::purl(x, files, output = newfile)
}

# -------------------------------------------------------------------------------