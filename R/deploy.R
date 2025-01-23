### shiny deployment


### packages
library(shinylive)

## set working directory
setwd("~/Documents/GitHub/pscShiny")


shinylive::export(appdir="test",destdir="docs")


httpuv::runStaticServer("docs/", port=8008)
