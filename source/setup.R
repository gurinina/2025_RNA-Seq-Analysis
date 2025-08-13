# Global options for code visibility, formatting, and output width
options(width = 80)

knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE    # Suppress all warnings
)

library(knitr)
library(pheatmap)
library(bookdown)
library(DESeq2)
library(RColorBrewer)
library(tidyverse)
library(dplyr)
library(gplots)
library(ggrepel)
library(ggplot2)
library(GOenrichment)
library(GO.db)
library(fgsea)
library(enrichplot)
library(clusterProfiler)
library(org.Hs.eg.db)
library(purrr)
library(rmarkdown)
library(GOSemSim)
library(visNetwork)
library(igraph)
library(ggraph)


# servr::httd("_book")
# servr::daemon_stop(1)
