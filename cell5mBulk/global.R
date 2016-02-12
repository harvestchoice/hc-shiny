#####################################################################################
# Title: CRU and PDSI 1960-2013 Time Series across Districts
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# Load common libraries
library(data.table)
library(hcapi3)


# Load workspace
#setwd("./cell5mBulk")
load("./tmp/cell5mBulk.RData")

# Check on `hcapi3` package timestamp, and recreate bulk downloads as needed
tmp <- hcapi.version
hcapi.version <- packageVersion("hcapi3")

# Also check if files exist
f <- list.files("../assets/bulk", "*.csv", recursive=T)

# If not, re-create
if( tmp!=hcapi.version | length(f)==0 ) {

  # Clean up
  unlink("../assets/bulk/*")

  # Package by category, omitting admin variables
  vi <- vi[order(cat1, cat2, cat3, varCode)][published==T]
  cat <- vi[varCode %in% g, unique(cat2)]
  cat <- vi[!cat2 %in% cat, unique(cat2)]

  for( i in cat ) {
    vars <- vi[cat2==i, varCode]
    # Generate CSV, STATA, and netCDF downloads with README, LICENSE
    for( f in c("csv", "dta", "asc") ) {
      tmp <- hcapi(vars, format=f, path=tempdir())
      path <- paste0("../assets/bulk/", gsub(" ", "_", tolower(i), fixed=T), "-",
        format(Sys.Date(), "%y.%m.%d"), ".", f, ".zip")
      zip(path, tmp, flags="-9Xjm", zip="zip")
    }
  }


  # Create persistent table of download links
  f <- list.files("../assets/bulk")
  t <- c("ESRI ASCII Grid (.asc)", "comma-separated values (.csv)", "STATA 12 (.dta)")

  hcapi.bulk <- vi[cat2 %in% cat, .(.N,
    desc=paste(paste(sample(varLabel, (min(3, length(varLabel)))), collapse="<br>"), "..."),
    url=paste0('<a href="http://tools.harvestchoice.org/assets/bulk/',
      f[gsub("_", " ", f, fixed=T) %like% tolower(cat2)][1:3], '">', t, '</a>', collapse='<br>')
  ), keyby=.(cat1, cat2)]

  hcapi.bulk <- split(hcapi.bulk, hcapi.bulk$cat1)
}

save(hcapi.version, hcapi.bulk, file="./tmp/cell5mBulk.RData")
