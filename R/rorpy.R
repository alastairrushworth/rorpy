#' @title R or Python?  Classification of webpages by code content.
#' @param A character vector of length n containing urls.
#' @return A tibble containing the probability that the input url contains R 
#' code (column \code{r}), Python code (column \code{py}) or another code type 
#' (column \code{other}).  If no code can be found the probability vector will 
#' be 0.  If there are problems fetching data from a url, the NAs will be returned 
#' instead of classification probabilities.
#' @description This function fetches code chunks from a vector of urls (where 
#' code is assumed to be tagged \code{<pre>}, \code{<code>} or \code{<textarea>}).   
#' The function uses a pretrained random forest classifier to calculates the 
#' probability that the code is R, Python or neither.
#' @examples 
#' # not run:
#' # rorpy("https://google.com") # no code here...
#' # rorpy("http://dplyr.tidyverse.org") # 99\% sure it's R.......  
#' # rorpy("https://keras.io") # also about 99\% sure it's python
#' @export
#' @import randomForest
#' @importFrom rvest guess_encoding
#' @importFrom stats predict
#' @importFrom tibble tibble
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom xml2 read_html


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
    # try to guess the encoding of the webpage
    enc      <- guess_encoding(url[i])$encoding[1]
    # attempt to read url using guessed encoding
    x        <- try(read_html(url[i], encoding = enc), silent = T)
    if(!"try-error" %in% class(x)){
      code <- get_code_text(x)
      # if there is anything left attempt to classify it
      if(nchar(code) > 0){
        # get the number of occurences of certain syntaxes
        xout           <- get_text_features(code)
        # predict probability of content being 
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