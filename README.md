
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {knitrcodegetlive}

knitr::knit\_code$get has been used in projects like flair and
flipbookr… (where else?).

In the {flair} package, you can also reference code chunks in your
interactive session (RStudio). This is really cool\! It is powerful for
users to quickly inspect what will happen upon compiling - without
compiling\! This may allow for *much* faster iteration. Also, users in
general expect their interactive session to mirror what happens in their
rendered doc. Meeting that expectation is good.

Here, using the flair strategies and code (I was blown away\!), we
propose ‘return\_chunk\_code’ for general use\!

I’m excited to use it in flipbookr (my live would have been so much
better\!) and readme2pkg.

# hesitations

1.  where’d be good for code to live?
2.  How is the live bit tested in a systematic way.
3.  other pitfalls?

<!-- end list -->

``` r
sample_code <- 
'library(tidyverse)
iris %>%
  group_by(Species) %>%
  summarize(mean(Sepal.Length))'
```

``` r
library(tidyverse)
```

    #> ── Attaching core tidyverse packages ─────────────────── tidyverse 2.0.0.9000 ──
    #> ✔ dplyr     1.1.0     ✔ readr     2.1.4
    #> ✔ forcats   1.0.0     ✔ stringr   1.5.0
    #> ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    #> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    #> ✔ purrr     1.0.1     
    #> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    #> ✖ dplyr::filter() masks stats::filter()
    #> ✖ dplyr::lag()    masks stats::lag()
    #> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
iris %>%
  group_by(Species) %>%
  summarize(mean(Sepal.Length))
```

    #> # A tibble: 3 × 2
    #>   Species    `mean(Sepal.Length)`
    #>   <fct>                     <dbl>
    #> 1 setosa                     5.01
    #> 2 versicolor                 5.94
    #> 3 virginica                  6.59

```` r
# Awesome!
check_is_live <- function(){
  
  is_live <- FALSE
  
  # Check to see if we're in editor context
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {

    is_live <- tryCatch({
      rstudioapi::getSourceEditorContext()
      TRUE
    }, error = function(e) FALSE)

  }  
  
  return(is_live)
  
}

# so cool!
chunk_extract <- function(.contents, chunk_name) {

  # Find the start of the desired chunk
  chunk_regex <- paste0('\\`\\`\\`\\{[A-z]+ ', chunk_name, '(\\}|(,.*\\}))$')

  start_chunk <- .contents %>%
    str_which(chunk_regex)

  if (length(start_chunk) == 0) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

  } else if (length(start_chunk) > 1) {

    stop(paste0("Error: Duplicate chunk name '", chunk_name, "'"))

  }

  end_chunk <- .contents[-c(1:start_chunk)] %>%
    str_which(fixed("```")) %>%
    min() + start_chunk

  chunk_text <- .contents[(start_chunk):(end_chunk)] %>%
    str_c(collapse = "\n")

  attributes(chunk_text) <- NULL

  return(chunk_text)

}

chunk_remove_fencing_and_options <- function(code_chunk){
  
  # does not yet, in fact, remove options like these: 
  # | my-chunk, echo = FALSE, fig.width = 10,
  # | fig.cap = "This is a long long
  # |   long long caption."
  
 chunk_as_vec <- str_split(code_chunk,"\\n")[[1]] 
 
 # remove fencing which are first and last lines
 return(chunk_as_vec[2:(length(chunk_as_vec)-1)])
  
}

# wow!
return_chunk_code_live <- function(chunk_name) {

  
    ed             <- rstudioapi::getSourceEditorContext()
    source         <- ed$contents
    
    my_code_chunk  <- chunk_extract(source, chunk_name)

    # If neither of those worked, error
    if (is.null(my_code_chunk)) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

    }

    # remove chunk fencing, first and last lines
    my_code <- chunk_remove_fencing_and_options(my_code_chunk)
    
    return(my_code)
  
}

#' Title
#'
#' @param chunk_name a character string with the name of the chunk of interest
#'
#' @return a vector of the code contained in the referenced chunk
#' @export 
#'
#' @examples
return_chunk_code <- function(chunk_name){
  
  is_live <- check_is_live()
  
  if(is_live){
    return_chunk_code_live(chunk_name)
  }else{
  knitr::knit_code$get(name = chunk_name) %>% as.vector()
    }

}
````

``` r
library(tidyverse)
return_chunk_code("sample_chunk")
```

    #> [1] "library(tidyverse)"              "iris %>%"                       
    #> [3] "  group_by(Species) %>%"         "  summarize(mean(Sepal.Length))"

-----

# Package up?

``` r
devtools::create(".")
usethis::use_lifecycle_badge("experimental")
usethis::use_pipe()
```

``` r
# I'd love to run this live... but would need to incorporate live option
# readme2pkg is actually a big motivator for digging in to flair strategy
readme2pkg::chunk_to_r("flairliverework")
```

<https://github.com/EvaMaeRey/readme2pkg>

### obviously lots to do if committing to this.

``` r
devtools::check()
```
