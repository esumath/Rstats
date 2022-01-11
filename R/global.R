#library(config)
#library(golem)
requiredPackages = c('pkgload', 'markdown', 'BSDA', 'car', 'datarium', 'dplyr', 'DT', 'EnvStats', 'formatR', 'ggplot2', 'methods', 'rclipboard', 'rhandsontable', 'rstatix', 'shiny', 'shinyAce', 'shinyjs', 'shinymeta', 'shinythemes', 'utils', 'waiter')
for(p in requiredPackages)
{
if(!require(p,character.only = TRUE)) install.packages(p)
library(p,character.only = TRUE)
}

options(shiny.maxRequestSize = 10*1024^2)
clean_readlines <- function(file) {
  return(tidy_source(file, output = FALSE)$text.tidy)
}

#as.numeric( unlist(strsplit(text,",")) )
# text="1/3, 1/3,1/3"
# as.numeric(sapply( unlist(strsplit(text,",")), function(x) eval(parse(text = x))) )
extract <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  #as.numeric(split)
  as.numeric( sapply(split, function(x) eval(parse(text = x))) )
}

numericNames <- function(data) {
  vnames<-colnames(data)
  vtypes <- vapply(data, class, character(1))
  data_vars<-data.frame(vnames = vnames, vtypes = vtypes, stringsAsFactors = FALSE)
  data_vars <- data_vars[data_vars$vtypes == "numeric" | data_vars$vtypes == "integer", ]
  vnames <- data_vars$vnames
  return(vnames)
}
categoricNames <- function(data) {
  vnames<-colnames(data)
  vtypes <- vapply(data, class, character(1))
  data_vars<-data.frame(vnames = vnames, vtypes = vtypes, stringsAsFactors = FALSE)
  data_vars <- data_vars[data_vars$vtypes == "factor" | data_vars$vtypes == "character", ]
  vnames <- data_vars$vnames
  return(vnames)
}

as_call <- function(x) {
  if (inherits(x, "formula")) {
    stopifnot(length(x) == 2)
    x[[2]]
  } else if (is.atomic(x) || is.name(x) || is.call(x)) {
    x
  } else {
    stop("Unknown input")
  }
}

interpolate <- function(code, ..., mydir, `_env` = parent.frame(),
                        file = "code_all.R", 
                        save_result = FALSE, append = FALSE,  eval = TRUE){
  stopifnot(inherits(code, "formula"), length(code) == 2)
  args <- lapply(list(...), as_call)
  expr <- methods::substituteDirect(as_call(code), args)
  cat(paste0(as.character(expr)[2], "\n"), 
      file = file.path(mydir, file), append = append)
  if (save_result) cat(paste0(paste(readLines(file.path(mydir, file)), 
      collapse = "\n"), "\n"), file = file.path(mydir, "code_all.R"), append = TRUE)
  if (eval) eval(expr, `_env`)
}

interpolate2 <- function(code, ..., `_env` = parent.frame()) {
  stopifnot(inherits(code, "formula"), length(code) == 2)
  args <- lapply(list(...), as_call)
  expr <- methods::substituteDirect(as_call(code), args)
  eval(expr, `_env`)
}

interpolate3 <- function(code, ..., `_env` = parent.frame()) {
  stopifnot(inherits(code, "formula"), length(code) == 2)
  args <- lapply(list(...), as_call)
  expr <- methods::substituteDirect(as_call(code), args)
  print(expr)
  eval(expr, `_env`)
}
