
rorpy <- function(url){
  x   <- try(read_html(url), silent = T)
  if(!"try-error" %in% class(x)){
    # combine pre, code and text area tags
    pre_tags  <- html_text(html_nodes(x, "pre"))
    code_tags <- html_text(html_nodes(x, "code"))
    area_tags <- html_text(html_nodes(x, "textarea"))
    code      <- c(pre_tags, code_tags, area_tags)
    # remove everything between # and \n - these are usually comments
    code <- gsub("#.*?\n", " ", code)
    # split by \n
    code <- unlist(strsplit(code, "\n"))
    # remove letter R - seems common in some cases
    code <- code[!code %in% c("R")]
    # remove \r code
    code <- paste(gsub("\r", "", code), collapse = " ")
    # if there is anything left attempt to classify it
    if(nchar(code) > 0){
      # get the number of occurences of certain syntaxes
      items <- c(" = ", "==",  "<-", "::", ";",  "\\.", "%>%", "\\(\\)", "\\(", "\\)", 
                 "\\[", "\\]", "\\[\\[", "\\]\\]", "__", "_", "\\[\\]", "\\{", "\\}", "\\{\\}", 
                 "library", "import", "numpy", "from", "pandas", "as", "\\[0\\]", "\\.py", 
                 "scipy", "require\\((.*?)\\)", "library\\((.*?)\\)", "def ", "fileimport", 
                 "\\$", "geom_", "\\((.*?)\\)", "install.packages\\((.*?)\\)", "\\[(.*?)\\]", 
                 "plot\\((.*?)\\)", "\\~", "function\\((.*?)\\)", "\\+", "%\\*%", "apply", 
                 " c\\((.*?)\\)")
      # count up the number of occurences of the patterns above
      xout           <- as.data.frame(t(str_count(code, items)))
      colnames(xout) <- items
      pred_out       <- predict(ft, xout, type = "prob")
    } else { 
      pred_out       <- data.frame(other = 0, py = 0, r = 0)
      }
  } else {
    stop("There was a problem reading the specified url")
  }
  return(as_tibble(pred_out))
}
