library(brew)
library(rmarkdown)

library(magrittr)
library(jsonlite)
library(xml2)

library(rcellminerUtilsCDB)
library(rcellminer)

# PARAMETERS ----
workDir <- "."

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

# LOAD DATA ----
data("cellLineMatchTab")

srcContent <- readRDS(file.path(workDir, "srcContent.rds"))
# mainDataset <- "nci60"
# cellLines <- cellLineMatchTab$nci60
mainDataset <- "cellosaurus_identifier"
#cellLineMatchTab <- read.table("tmp_cellline_match.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)
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

# CREATE SITEMAP ----
output_dir <- "_static"
base_url <- "https://discover.nci.nih.gov/cellminercdb/cell_lines/"
file_list_html <- list.files(file.path(workDir, output_dir), "html")
file_list_img <- list.files(file.path(workDir, output_dir), "png", recursive=TRUE)

# Simple Sitemap (without images)
#writeLines(paste0(base_url, file_list_html), file.path(workDir, output_dir, "sitemap.txt"))

# Sitemap XML Format Description: https://www.sitemaps.org/protocol.html
doc <- xml_new_root("urlset", 
                    "xmlns"="http://www.sitemaps.org/schemas/sitemap/0.9", 
                    "xmlns:image"="http://www.google.com/schemas/sitemap-image/1.1") 

for(page in file_list_html) {
  # Get a string to match against the images
  page_prefix <- file_path_sans_ext(page)
  
  url <- doc %>% xml_add_child("url")
  
  loc <- url %>% xml_add_child("loc")
  xml_text(loc) <- paste0(base_url, page)
  
  lastmod <- url %>% xml_add_child("lastmod")
  xml_text(lastmod) <- Sys.Date() %>% as.character
  
  changefreq <- url %>% xml_add_child("changefreq")
  xml_text(changefreq) <- "monthly"
  
  if(page_prefix != "index") {
    image <- url %>% xml_add_child("image") %>% xml_set_namespace("image")
    
    # Add correct image
    tmp_image <- image %>% xml_add_child("loc") %>% xml_set_namespace("image")
    idx <- grep(page_prefix, file_list_img)
    image_prefix <- file_list_img[idx]
    xml_text(tmp_image) <- paste0(base_url, image_prefix)
    
    tmp_image <- image %>% xml_add_child("title") %>% xml_set_namespace("image")
    xml_text(tmp_image) <- image_prefix %>% basename %>% file_path_sans_ext %>% gsub("_", " ", .) %>% gsub('.{0,2}$', '', .)    
  }
}

invisible(write_xml(doc, file.path(workDir, output_dir, "sitemap.xml")))
length(file_list_html)

#source("generate_tumorcomparer_static_rmds.R")

# CLEANUP ----
file.remove(list.files(pattern="*_cellminercdb.[Rmd|html]", full.names=TRUE))
#file.remove(list.files(pattern="*_cellminercdb_files", full.names=TRUE))
