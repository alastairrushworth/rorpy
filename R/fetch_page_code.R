fetch_page_code <- function(url){
  # try to guess the encoding of the webpage
  enc      <- try(guess_encoding(url)$encoding[1], silent = T)
  # attempt to read url using guessed encoding
  x        <- try(read_html(url, encoding = enc), silent = T)
  # return error if there was a problem
  if("tryerror" %in% class(x)) stop("Problem fetching url.  Check url is valid.")
  # url is ok, then get code text
  get_code_text(x)
}