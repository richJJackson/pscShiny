### shiny deployment


### packages
library(shinylive)

## set working directory
setwd("~/Documents/GitHub/pscShiny")

file_path <- "docs/"
f <- list.files(file_path, include.dirs = F, full.names = T, recursive = T)
file.remove(f)


shinylive::export(appdir="test",destdir="docs")

httpuv::runStaticServer("docs")
httpuv::runStaticServer("docs/", port=8008)
