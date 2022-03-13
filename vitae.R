## packages used
library(readxl)
library(bd)

## helper functions
tag  <- function(x, tag) paste0("\\", tag, "{", x, "}")
gtag <- function(x, y, tag) gsub(y, tag(y, tag), x, fixed = TRUE)
trim <- function(x) gsub("^\\s+|\\s+$|\\.$", "", x)
initial <- function(x) {
  y <- strsplit(x, "-")[[1]]
  ints <- sapply(y, function(x) substr(x, 1, 1))
  int <- paste0(ints, collapse = "-")
  return(int)
}
abbr <- function(x) {
  x_split <- strsplit(x, ",", fixed = TRUE)
  x_split[[1]][2] <- trim(x_split[[1]][2])
  names_vector <- strsplit(x_split[[1]][2], " ", fixed = TRUE)[[1]]
  names_vector <- sapply(names_vector, initial, USE.NAMES = FALSE)
  author <- paste0(x_split[[1]][1], " ",
                   paste(names_vector, collapse = ""))
  author <- sanitize_specials(author, "latex")
  if (is.na(x_split[[1]][2])) author <- substr(author, 0, nchar(author) - 3)
  if (x_split[[1]][1] == "Devleesschauwer") {
    author <- tag(author, "textbf")
  }
  return(author)
}

## needle
needle <-
  read.table("needle.txt", stringsAsFactors = FALSE, sep = "\t")$V1

##
##

print_article <-
  function(item, id) {
    ## reformat authors string
    authors_string <- strsplit(item$authors, ";")[[1]]
    authors_string <- sapply(authors_string, trim, USE.NAMES = FALSE)
    authors_string <- sapply(authors_string, abbr, USE.NAMES = FALSE)
    
    ## reformat year string
    year_string <- item$year
    if (is.null(item$year) || is.na(item$year)){
      year_string <- "."
    } else {
      year_string <- paste0(" (", item$year, ") ")

    }
    
    ## reformat title string
    title_string <- item$title

    for (i in seq_along(needle))
      title_string <- gtag(title_string, needle[i], "textit")
    
    if (substring(title_string, nchar(title_string)) != "?")
      title_string <- paste0(title_string, ".")

    ## reformat journal string
    journal_string <- gsub(".", "", item$journal_short, fixed = TRUE)
    journal_string <- tag(journal_string, "textit")

    ## reformat volume/page string
    if (is.null(item$year) || is.na(item$year)) {
      vp_string <- ""
      
    } else if (is.na(item$volume)) {
      vp_string <- ", in press"
      
    } else if (!is.na(item$eID)) {
      vp_string <- paste0(" ", item$volume, ":", item$eID)
      
    } else {
      vp_string <- paste0(" ", item$volume, ":", item$from, "-", item$to)
    }
    
    ## reformat DOI string
    doi_string <- item$DOI
    if (!is.null(doi_string) && !is.na(doi_string)){
      doi_string <- paste0(". doi: \\href{https://doi.org/",
                           doi_string, "}{", doi_string, "}")
    } else {
      doi_string <- ""
    }
    
    ## reformat DOI preprint string
    doi_preprint_string <- item$doi_preprint
    if (!is.null(doi_preprint_string) && !is.na(doi_preprint_string)){
      doi_preprint_string <-
        paste0(". preprint: \\href{https://doi.org/",
               doi_preprint_string, "}{", doi_preprint_string, "}")
    } else {
      doi_preprint_string <- ""
    }

    cat("\\item[{[", id, "]}] ", sep = "")
    cat(paste(authors_string, collapse = ", "))
    cat(year_string, " ", sep = "")
    cat(title_string, " ", sep = "")
    cat(journal_string)
    cat(vp_string)
    cat(doi_string)
    cat(doi_preprint_string)
  }


print_chapter <-
  function(item, id) {
    ## reformat authors string
    authors_string <- strsplit(item$authors, ";")[[1]]
    authors_string <- sapply(authors_string, trim, USE.NAMES = FALSE)
    authors_string <- sapply(authors_string, abbr, USE.NAMES = FALSE)

    ## reformat editors string
    editors_string <- strsplit(item$editors, ";")[[1]]
    editors_string <- sapply(editors_string, trim, USE.NAMES = FALSE)
    editors_string <- sapply(editors_string, abbr, USE.NAMES = FALSE)
    ed <- ifelse(length(editors_string) == 1, "ed", "eds")
    editors_string <-
      paste0("In: ", paste(editors_string, collapse = ", "), " (", ed, ") ")
    
    ## reformat year string
    year_string <- item$year
    if (!is.na(item$year)){
      year_string <- paste0(" (", item$year, ") ")
    } else {
      year_string <- "."
    }
    
    ## reformat title string
    title_string <- item$title
    
    for (i in seq_along(needle))
      title_string <- gtag(title_string, needle[i], "textit")
    
    if (substring(title_string, nchar(title_string)) != "?")
      title_string <- paste0(title_string, ".")
    
    ## reformat journal string
    book_string <- item$book
    book_string <- tag(book_string, "textit")
    
    ## reformat volume/page string
    if (is.na(item$year)) {
      vp_string <- ""
      
    } else if (is.na(item$from)) {
      vp_string <- ", in press"
      
    } else {
      vp_string <- paste0(", pp ", item$from, "-", item$to)
    }
    
    ## reformat DOI string
    doi_string <- item$DOI
    if (!is.null(doi_string) && !is.na(doi_string)){
      doi_string <- paste0(". doi: \\href{https://doi.org/",
                           doi_string, "}{", doi_string, "}")
    } else {
      doi_string <- ""
    }
    
    cat("\\item[{[", id, "]}] ", sep = "")
    cat(paste(authors_string, collapse = ", "))
    cat(year_string, " ", sep = "")
    cat(title_string, " ", sep = "")
    cat(editors_string)
    cat(book_string)
    cat(vp_string)
    cat(doi_string)
  }

print_student <-
  function(item, id) {
    ## reformat promoters string
    pr_string <- strsplit(item$promoters, ";")[[1]]
    pr_string <- sapply(pr_string, trim, USE.NAMES = FALSE)
    pr_string <- sapply(pr_string, abbr, USE.NAMES = FALSE)
    
    ## define promoter(s)
    pr <- ifelse(length(pr_string) == 1,
                 "Promoter:", "Promoters:")
    
    ## reformat year string
    year_string <- sprintf("(%s)", item$year+1)
    
    ## reformat title string
    title_string <- item$title

    for (i in seq_along(needle))
      title_string <- gtag(title_string, needle[i], "textit")

    ## print item
    cat("\\item ", item$student, " ", year_string, " ", sep = "")
    cat(title_string, ". ", sep = "")
    cat(item$grade, " ", item$topic, ", ", item$univ, ". ", sep = "")
    cat(pr, " ", paste(pr_string, collapse = ", "), ".\n", sep = "")
  }


print_proceeding <-
  function(item, id) {
    ## reformat authors string
    authors_string <- strsplit(item$authors, ";")[[1]]
    authors_string <- sapply(authors_string, trim, USE.NAMES = FALSE)
    authors_string <- sapply(authors_string, abbr, USE.NAMES = FALSE)
    
    ## reformat year string
    year_string <- item$year
    if (!is.na(item$year)){
      year_string <- paste0(" (", item$year, ") ")
    } else {
      year_string <- "."
    }
    
    ## reformat title string
    title_string <- item$title

    for (i in seq_along(needle))
      title_string <- gtag(title_string, needle[i], "textit")

    title_string <- sanitize_specials(title_string, "latex")

    if (substring(title_string, nchar(title_string)) != "?")
      title_string <- paste0(title_string, ".")

    ## reformat conference string
    conf <- sanitize_specials(item$conference, "latex")
    location <- sanitize_specials(item$location, "latex")

    conf_string <-
      paste0("Presented at the \\emph{", conf, "}",
             "; ", item$day, " ", month.abb[item$month], " ", item$year,
             "; ", location, ".")
    
    cat("\\item[{[", id, "]}]", sep = "")
    cat(paste(authors_string, collapse = ", "))
    cat(year_string, " ", sep = "")
    cat(title_string, " ", sep = "")
    cat(conf_string)
    cat("\n")
  }


print_package <-
  function(item, id) {
    ## reformat authors string
    authors_string <- strsplit(item$authors, ";")[[1]]
    authors_string <- sapply(authors_string, trim, USE.NAMES = FALSE)
    authors_string <- sapply(authors_string, abbr, USE.NAMES = FALSE)
    
    ## reformat title string
    title_string <- item$title
    
    for (i in seq_along(needle))
      title_string <- gtag(title_string, needle[i], "textit")
    
    title_string <- sanitize_specials(title_string, "latex")
    
    if (substring(title_string, nchar(title_string)) != "?")
      title_string <- paste0(title_string, ".")
    
    cat("\\item[{[", id, "]}] ", sep = "")
    cat(paste(authors_string, collapse = ", "))
    cat( " (", format(item$date, "%Y"), ") ", sep = "")
    cat("\\textsf{", item$package, "}: ", sep = "")
    cat(title_string)
    cat(" R package version ", item$version, ". ", sep = "")
    cat("\\url{", item$url, "}", sep = "")
    cat("\n")
  }
