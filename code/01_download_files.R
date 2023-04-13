library(downloader)
source(here::here("code","00_parameters.R"))

finame_places <- here::here("data",finame_cdc_places)
if(!file.exists(finame_places)) stop(paste0("CDC PLACES dataset must be downloaded manually: ", url_cdc_places))

finame_ces4 <- here::here("data",finame_ces_data)
if(!file.exists(finame_ces4)) stop(paste0("CES 4.0 dataset must be downloaded and unzipped manually: ", url_ces4))

print("Downloading CES 3.0 data file")
download(url_ces3, here::here("data", finame_ces3))

print("Downloading census block information")
download(url_blocks, dest="data/blocks.zip", mode="wb") 
unzip ("data/blocks.zip", exdir = "./data")

print("Downloading official designations for all GGRF earmarked funding according to AB 1550")
download(url_designation, here::here("data/", basename(url_designation)))

print("Downloading funding dataset")
download(url_funding, here::here("data", basename(url_funding)))

print("Downloading housing burden file")
download(url_hb, here::here("data", basename(url_hb)))

if(!file.exists(finame_chas)) stop(paste0("CHAS dataset must be downloaded and extracted into data diretory manually: ", url_chas))

