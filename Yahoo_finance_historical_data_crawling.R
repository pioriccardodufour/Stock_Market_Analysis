## Crawl historical data from yahoo finance

#Load list of stock ids
id <- read_excel("data_ids_source2_50000.xlsx")


for (n in 3451:50000){
  print(n)
  
#Create url where file is downloaded
url <- paste("https://query1.finance.yahoo.com/v7/finance/download/",id[[1]][n],"?period1=946684800&period2=1638576000&interval=1d&events=history&includeAdjustedClose=true", sep="")
#url

if (url.exists(url) == TRUE){
  # Create file on drive where data will be stored
  destfile <- paste("Historical Data/",id[[1]][n],".csv", sep = "")
  destfile

  #Download file
  file.create(destfile)
  
  #Check if access is denied
  html <- GET(url, followlocation = TRUE)
  doc = htmlParse(html, asText=TRUE)
  if (regexpr("Unauthorized",xpathSApply(doc, "//p", xmlValue))>0){
    print("Page temporarily not accessible")
    date_time<-Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time))<280){} #dummy while loop
  }
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<3){} #dummy while loop: Reduce requests per minute
  download.file(url, destfile, quiet = TRUE)

}
}
