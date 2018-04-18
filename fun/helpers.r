list.of.packages <- c("ggplot2","geoR","ggthemes","RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repo="http://cran.rstudio.com/")
