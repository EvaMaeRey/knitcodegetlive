
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

I’m excited to use it in flipbookr (my life would have been so much
better\!) and readme2pkg.

# hesitations

1.  where’d be good for code to live?
2.  if a package, a better name?
3.  How is the live bit tested in a systematic way.
4.  other pitfalls?

#### contents of a chunk called ‘sample\_chunk’:

``` r
library(tidyverse)
iris |>
  group_by(Species) |>
  summarize(mean(Sepal.Length))
```

# flair code for grabbing chunk code live rework\!

I the rework, I’m just trying to understand what’s going on so braking
things up more, and make the code potentially usable in other contexts.

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
text_chunk_extract <- function(.text, chunk_name) {

  # Find the start of the desired chunk
  chunk_regex <- paste0('\\`\\`\\`\\{[A-z]+ ', chunk_name, '(\\}|(,.*\\}))$')

  start_chunk <- .text |>
    stringr::str_which(chunk_regex)

  if (length(start_chunk) == 0) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

  } else if (length(start_chunk) > 1) {

    stop(paste0("Error: Duplicate chunk name '", chunk_name, "'"))

  }

  end_chunk <- .text[-c(1:start_chunk)] |>
    stringr::str_which(stringr::fixed("```")) |>
    min() + start_chunk

  chunk_text <- .text[(start_chunk):(end_chunk)] |>
    stringr::str_c(collapse = "\n")

  attributes(chunk_text) <- NULL

  return(chunk_text)

}

chunk_remove_fencing_and_options <- function(code_chunk){
  
  # does not yet, in fact, remove options like these: 
  # | my-chunk, echo = FALSE, fig.width = 10,
  # | fig.cap = "This is a long long
  # |   long long caption."
  
 chunk_as_vec <- stringr::str_split(code_chunk,"\\n")[[1]] 
 
 # remove fencing which are first and last lines
 return(chunk_as_vec[2:(length(chunk_as_vec)-1)])
  
}

# wow!
return_chunk_code_live <- function(chunk_name) {

  
    ed        <- rstudioapi::getSourceEditorContext()
    source    <- ed$contents

    # can we use knitr tools to directly parse source for us? 
    # tmp       <- tempfile()
    # writeLines(source, tmp)
    # readLines(tmp)
    # knitr::knit_code$get(name = tmp)
    
    my_code_chunk  <- text_chunk_extract(.text = source, chunk_name)

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
  knitr::knit_code$get(name = chunk_name) |> as.vector()
    }

}
````

``` r
library(tidyverse)
return_chunk_code("sample_chunk")
```

    #> [1] "library(tidyverse)"              "iris |>"                        
    #> [3] "  group_by(Species) |>"          "  summarize(mean(Sepal.Length))"

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
usethis::use_package("stringr")
usethis::use_package("rstudioapi")
usethis::use_package("knitr")
usethis::use_mit_license()
```
