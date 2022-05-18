library(brew)
library(rmarkdown)

library(magrittr)
library(jsonlite)

library(rcellminerUtilsCDB)
library(rcellminer)

#data("cellLineMatchTab")

# FUNCTIONS ----
slugify <- function(x) {
  x <- gsub("[^[:alnum:] ]", "", x)
  x <- gsub(" ", "_", x)
  x <- tolower(x)
  return(x)
}

# Function to generate report and render output
create.report <- function(reports, prepend=NULL) {
  # Set parameters 
  filePrefix <- slugify(reports$cellLine)

  mainDataset <- mainDataset
  curCellLine <- reports$cellLine
    
  chunkPlotCaption <- paste0("Cell Line ", curCellLine, " Available Pharmacogenomics Datasets")
  chunkPlotName <- tolower(gsub(" ", "_", chunkPlotCaption))
  
  filenamePrefix <- paste0(filePrefix, "_cellminercdb")
  htmlFile <- paste0(filenamePrefix, ".html")
  rmdFile <- file.path(workDir, paste0(filenamePrefix, ".Rmd"))
  
  #DEBUG
  cat(".Rmd: ", rmdFile, "\n")
  
  brew("cellminercdb_template.brew", rmdFile)
}

# PARAMETERS ----
workDir <- "."

# LOAD DATA ----
srcContent <- readRDS(file.path(workDir, "srcContent.rds"))
# mainDataset <- "nci60"
# cellLines <- cellLineMatchTab$nci60
mainDataset <- "cellosaurus_identifier"
cellLineMatchTab <- read.table("tmp_cellline_match.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)
restrictedDatasets <- c(names(srcContent), mainDataset) %>% unique

# Remove cell lines with no data in srcContent
tmp <- cellLineMatchTab[, names(srcContent)]
cellLineMatchTab <- cellLineMatchTab[rowSums(is.na(tmp)) != ncol(tmp), restrictedDatasets]
nrow(cellLineMatchTab)

# Remove cell lines with no cellosaursus entry
cellLineMatchTab <- cellLineMatchTab[!is.na(cellLineMatchTab$cellosaurus_identifier), ]
#cellLineMatchTab <- cellLineMatchTab[!is.na(cellLineMatchTab$cellosaurus_identifier) & !is.na(cellLineMatchTab$nci60),]

# Remove duplicated entries
# FIXME: Fix this in the original table
idx <- !(duplicated(cellLineMatchTab$cellosaurus_identifier) | duplicated(cellLineMatchTab$cellosaurus_identifier, fromLast=TRUE))
cellLineMatchTab <- cellLineMatchTab[idx,]
  
cellLines <- cellLineMatchTab[,mainDataset]
cellLines <- cellLines[!is.na(cellLines)]

# NOTE: dummy added 
reports <- data.frame(cellLine=cellLines, dummy=NA, stringsAsFactors=FALSE)

# GENERATE REPORTS ----
for(i in 1:nrow(reports)) {
#for(i in 1:3) {
  create.report(reports[i,])
}

# RENDER SITE ----
rmarkdown::render_site(input=workDir)

# CREATE SITEMAP ----
output_dir <- "_static"
base_url <- "https://discover.nci.nih.gov/cellminercdb/cell_lines/"
file_list <- list.files(output_dir, "html")
writeLines(paste0(base_url, file_list), "sitemap.txt")

#source("generate_tumorcomparer_static_rmds.R")

# CLEANUP ----
#file.remove(list.files(pattern="*_cellminercdb.[Rmd|html]", full.names=TRUE))
#file.remove(list.files(pattern="*_cellminercdb_files", full.names=TRUE))
