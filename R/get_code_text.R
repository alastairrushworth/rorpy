#' @importFrom rvest html_nodes
#' @importFrom rvest html_text

get_code_text <- function(x){
  # combine pre, code and text area tags
  pre_tags  <- html_text(html_nodes(x, "pre"))
  code_tags <- html_text(html_nodes(x, "code"))
  area_tags <- html_text(html_nodes(x, "textarea"))
  code      <- c(pre_tags, code_tags, area_tags)
  # remove everything between # and \n - these are usually comments
  code <- gsub("#.*?\n", " ", code)
  # split by \n
  code <- unlist(strsplit(code, "\n"))
  # remove \r code
  code <- paste(gsub("\r", "", code), collapse = " ")
  # return code text
  code
}