#' @importFrom stringr str_count

get_text_features <- function(txt){
  # get the number of occurences of certain syntaxes
  items <- c(" = ", "==",  "<-", "::", ";",  "\\.", "%>%", "\\(\\)", "\\(", "\\)", 
             "\\[", "\\]", "\\[\\[", "\\]\\]", "__", "_", "\\[\\]", "\\{", "\\}", "\\{\\}", 
             "library", "import", "numpy", "from", "pandas", "as", "\\[0\\]", "\\.py", 
             "scipy", "require\\((.*?)\\)", "library\\((.*?)\\)", "def ", "fileimport", 
             "\\$", "geom_", "\\((.*?)\\)", "install.packages\\((.*?)\\)", "\\[(.*?)\\]", 
             "plot\\((.*?)\\)", "\\~", "function\\((.*?)\\)", "\\+", "%\\*%", "apply", 
             " c\\((.*?)\\)")
  # count up the number of occurences of the patterns above
  xout           <- as.data.frame(t(str_count(txt, items)))
  colnames(xout) <- items
  # return df of items
  xout
}
