
rorpy <- function(url){
  # number urls 
  n_urls   <- length(url)
  # set the output table format
  pred_df  <- tibble(r = numeric(), py = numeric(), other = numeric())
  # set a progress bar - useful if more than 1 url specified
  if(n_urls > 1) pb  <- txtProgressBar(min = 0, max = n_urls, style = 3)
  # loop over urls one by one
  for(i in 1:length(url)){
    # default prediction is NAs - this only kept if an error reading url
    pred_out <- rep(NA, 3)
    # attempt to read url
    x   <- try(read_html(url[i]), silent = T)
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
        pred_out       <- rev(as.numeric(predict(ft, xout, type = "prob")))
      } else { 
        pred_out       <- rep(0, 3)
      }
    }
    pred_df[i, ] <- pred_out
    if(n_urls > 1) setTxtProgressBar(pb, i)
  }
  if(n_urls > 1) close(pb)
  return(pred_df)
}
