_The code in this repo is no longer maintained - all webpage code inference functionality has been moved to [https://github.com/alastairrushworth/htmldf](https://github.com/alastairrushworth/htmldf)_

# rorpy
R or Python? Simple Classification of Webpages by Code Content

__installation__ 

```r
devtools::install_github("alastairrushworth/rorpy", dependencies = T)
```
__dplyr: a webpage with R code__

```r
rorpy("http://dplyr.tidyverse.org") # 99% sure it's R.......  

# A tibble: 1 x 3
    other      py     r
    <dbl>   <dbl> <dbl>
1 0.00450 0.00600 0.990
```
__Keras: a webpage with Python code__
```r
rorpy("https://keras.io") # also about 99% sure it's python

# A tibble: 1 x 3
    other    py       r
    <dbl> <dbl>   <dbl>
1 0.00950 0.988 0.00250
```

__Google: a webpage with no code tags__

```r
rorpy("https://google.com") # alas, there is no code to be had

# A tibble: 1 x 3
  other    py     r
  <dbl> <dbl> <dbl>
1    0.    0.    0.
```

