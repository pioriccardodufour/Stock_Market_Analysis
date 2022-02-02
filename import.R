#Importing the data from different Excel files and creating a unique dataset

library(readxl)
library(stringr)

setwd("C:\\Users\\Pio Riccardo Dufour\\Documents\\R\\RStudio\\Finance Project\\Historical Data")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read.csv)

start_date = "2016-01-01" #"2018-12-01"
end_date = "2020-12-31" #"2021-12-01"
time_period = as.numeric(as.Date(end_date) - as.Date(start_date)) + 1
stocks = data.frame()

# we want to keep data from 2018-12-01 to 2021-12-01
# if a company has less than 3 years of data I remove the whole time series from the list

deleted_elements = 0;
indices_to_remove = array()

for (x in 1:length(df.list)) {
  a = as.Date(df.list[[x]][[1]])[1]
  b = df.list[[x]][[1]][length(df.list[[x]][[1]])]
  
  if(a > start_date | b < end_date ) {
    
    indices_to_remove[deleted_elements + 1] = x
    file.list = file.list[-(x - deleted_elements)]
    deleted_elements = deleted_elements + 1
  }
}

for (y in rev(indices_to_remove)) {
  df.list[[y]] = NULL
}

for(x in 1:length(df.list)) { #I only keep the relevant data
  df.list[[x]][c(2,3,4,6,7)] = NULL
}

#I have to make a for loop in which I create at every iteration a temporary dataframe with the content
# of just one company. In the iteration I remove all the columns related to data prior 2018-12-01


for (k in 1:length(df.list)) {
  temp_stock = t(data.frame(df.list[[k]])) #eliminating data
  i = 1                                    #before starting date
  while (temp_stock[1,i] < start_date) {
    i = i + 1
  }
  temp_stock = temp_stock[,-1:-(i-1)]
  
  # creating the dataframe element by element,
  # filling in the missing values
  
  if(temp_stock[1,1] != as.Date(start_date)) {
    stocks[k,1] = temp_stock[2,1]
  }
  t = 0
  for (h in 1:time_period) {
    if(h != 1 & temp_stock[1,h-t] > as.Date(start_date) + h - 1) {
      stocks[k,h] = stocks[k,h-1]
      t = t + 1
    } else {
      stocks[k,h] = temp_stock[2,h-t]
    }
  }
}


days = array()
date = as.Date(start_date)
for (x in 1:time_period) {
  days[x] = as.character(date + x - 1)
}
colnames(stocks) = days
row.names(stocks) = gsub(".csv","", file.list)

# save and export dataset

setwd("C:\\Users\\Pio Riccardo Dufour\\Documents\\R\\RStudio\\Finance Project")
#write.csv(stocks, "stocks20162020.csv", row.names = TRUE)
#save(stocks, file = "stocks20162020.RData")



#USEFUL COMMANDS:
#stocks = stocks[,-1] #remove first column
#stocks = stocks[-1,] #remove first row

