
#
# http://iangow.wordpress.com/2011/08/07/r-code-to-fetch-fama-french-factor-data/
#

########################################################################
# Small program to fetch and organize Fama-French factor data.
########################################################################
 
# The URL for the data.
ff.url.partial <- paste("http://mba.tuck.dartmouth.edu",
                        "pages/faculty/ken.french/ftp", sep="/")
 
# Function to remove leading and trailing spaces from a string
trim <- function(string) {
    ifelse(grepl("^\\s*$", string, perl=TRUE),"",
                gsub("^\\s*(.*?)\\s*$","\\1", string, perl=TRUE))
}
 
################################################################################
#             First download Fama-French three-factor data                     #
################################################################################
 
# Download the data and unzip it
ff.url <- paste(ff.url.partial, "F-F_Research_Data_Factors_daily.zip", sep="/")
f <- tempfile()
download.file(ff.url, f)
file.list <- unzip(f, list=TRUE)
 
# Parse the data
ff_daily_factors <-
    read.fwf(unzip(f, files=as.character(file.list[1,1])),
             widths=c(8,8,8,8,10), header=FALSE,
             stringsAsFactors=FALSE, skip=5)
 
# Clean the data
for (i in 2:5) ff_daily_factors[,i] <- as.numeric(trim(ff_daily_factors[,i]))
for (i in 2:4) ff_daily_factors[,i] <- ff_daily_factors[,i]/100
names(ff_daily_factors) <- c("date", "mktrf", "smb", "hml", "rf")
ff_daily_factors$date <- as.Date(ff_daily_factors$date, format="%Y%m%d")
