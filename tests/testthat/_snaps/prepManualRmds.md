# prepManualRmds() reports a text reference bookdown will misread

    Code
      invisible(prepManualRmds("modules"))
    Message
      Copying module modRef1 ...
      Copying module modRef2 ...
    Condition
      Warning:
      prepManualRmds(): 2 line(s) begin with a text reference without being its own paragraph. bookdown reads each as a definition, so it may report a duplicated label that de-duplication here cannot remove -- doing so would delete the sentence. Re-wrap the module's prose so the label is not first on the line: modRef12.Rmd line 14 (ref:pct), modRef22.Rmd line 14 (ref:pct)

# a generated chapter has the expected shape

    Code
      cat(sub("root\\.dir = '.*'", "root.dir = '<moduledir>'", chapter), sep = "\n")
    Output
      
      # modT Module
      
      ```{r setup-modT, include = FALSE, eval = TRUE, cache = FALSE}
      knitr::opts_chunk$set(cache.rebuild = FALSE)
      knitr::opts_knit$set(SpaDES.docs.stageDir = '_manual_rmds/modT')
      knitr::opts_knit$set(root.dir = '<moduledir>')
      knitr::opts_chunk$set(echo = TRUE)
      ```
      
      Body text.
      
      (ref:key) a caption
      
      ## References
      \printbibliography[segment=\therefsegment,heading=none]

