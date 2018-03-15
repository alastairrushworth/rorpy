# rorpy
R or Python? Simple Classification of Webpages by Code Content

# installation
devtools::install_github("alastairrushworth/rorpy")

# usage - webpage with no code
rorpy("https://google.com") # alas, no code to be had

# usage - webpage with R code
rorpy("http://dplyr.tidyverse.org") # 
