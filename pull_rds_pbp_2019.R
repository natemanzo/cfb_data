## This is a very simple script to download an RDS file that contains the entire 2019 pbp data from cfbscrapR.
## Downloading this file instead of using the cfbscrapR function prevents overtaxing the API that cfbscrapR 
## calls to populate the data. It is also way faster! 

## The RDS file was compressed using the xz compression option in readr::write_rds()

## It takes about 20 seconds to read the file from github. I recommend saving the file to your desktop once you 
## download for the first time. Reading the file from your hard drive will take about a second.

t <- tempfile()
download.file("https://raw.githubusercontent.com/natemanzo/cfb_data/master/cfbscrapr_pbp_2019_raw_all.rds",t)
pbp_2019 <- readRDS(t)

## Uncomment the line below to SAVE the pbp data to your working directory
#readr::write_rds(pbp_2019, path = "pbp_2019_raw_all.rds")

## Uncomment the line below to READ the pbp data from your working directory
#pbp_2019 = readr::read_rds(path = "pbp_2019_raw_all.rds")

## Use this line if you're not sure what your working directory is
#getwd()