#####################################################################################
# Title:   Bulk Data Download
# Date:    February 2016
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


shinyServer(function(input, output, session) {

  output$tbl <- renderUI(

    lapply(names(hcapi.bulk), function(i) {
      tmp <- hcapi.bulk[[i]][, .SD, .SDcols=-1]
      setnames(tmp, c("Category", "Layers", "Description", "Download"))
      list(
        h2(i),
        renderTable(tmp, include.rownames=F,
          sanitize.text.function=function(x) x))
    })
  )
})


