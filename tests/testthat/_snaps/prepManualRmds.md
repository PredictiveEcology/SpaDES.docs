# a generated chapter has the expected shape

    Code
      cat(sub("root\\.dir = '.*'", "root.dir = '<moduledir>'", chapter), sep = "\n")
    Output
      
      # modT Module
      
      ```{r setup-modT, include = FALSE, eval = TRUE, cache = FALSE}
      knitr::opts_chunk$set(cache.rebuild = FALSE)
      knitr::opts_knit$set(root.dir = '<moduledir>')
      knitr::opts_chunk$set(echo = TRUE)
      ```
      
      Body text.
      
      (ref:key) a caption
      
      ## References
      \printbibliography[segment=\therefsegment,heading=none]

