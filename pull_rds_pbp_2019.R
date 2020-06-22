## This is a very simple script to download an RDS file that contains the entire 2019 pbp data from cfbscrapR.
## Downloading this file instead of using the cfbscrapR function prevents overtaxing the API that cfbscrapR 
## calls to populate the data. It is also way faster! 
## The RDS file was compressed using the xz compression option in readr::write_rds()

t <- tempfile()
download.file("https://raw.githubusercontent.com/natemanzo/cfb_data/master/cfbscrapr_pbp_2019_raw_all.rds",t)
df <- readRDS(t)
