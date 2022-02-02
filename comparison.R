############# USER SETUP #############

# We want to estimate the probability of the stock value to increase more than x%
# x has been called "value"
# enter in the following variable the codes of the companies you want to compare

companies = c("PMC.F","HMI.F","KEE.F","3FO.F","MWI.F")

# C97,LYI,SHM were not found because they have not been on the stock exchange for 3 years

########################### loading data #########################
# load stocks, all_percentage_arrays, table_arrays

load("~/R/RStudio/Finance Project/stocks.RData")
load("~/R/RStudio/Finance Project/table_arrays.RData")

# the following data contains the whole global environment of the script "analysis"
load("~/R/RStudio/Finance Project/all_percentage_arrays.RData")


############################ code ################################
library(ggplot2)

table_arrays[,27] = row.names(stocks)
colnames(table_arrays)[27] = "Company"

#taking the indices referred to the position of the company in the dataframe, and saving them
j = 1
indices = array()

for (i in companies) {
  indices[j] = rownames(subset(table_arrays, table_arrays[,27] == i))
  j = j + 1
}

probabilities = data.frame()

for (index in indices) {
  j = 1
  probability = array()
  cases = sum(t(table_arrays[index,1:26])[1:26])
  
  for (i in t(table_arrays[index,1:26])[1:26]) {
    probability[j] = (i/cases)*100
    j = j + 1
  }
  probabilities = rbind(probabilities,probability) #to be continued
}
colnames(probabilities) = colnames(table_arrays)[1:26]

data = data.frame()
for (j in 1:length(companies)) {
  
  growth = c("-a30-","-b30","-c20","-d15","-e10","-f5","-g3","-h2","-i1","-j0.75","-k0.50","-l0.25","-m0.1",
                 "n0.1","o0.25","p0.50","q0.75","r1","s2","t3","u5","v10","w15","x20","y30","z30+")
  prob = t(probabilities[j,])[1:26]
  comp = rep(companies[j],26)
  temp = data.frame(growth, prob, comp)
  data = rbind(data, temp)
}

#comparison between companies
title = paste("Probability of having a certain stocks price growth/drop based on the frequency of historical daily growths and drops")

ggplot(data, aes(x = growth, y = prob, colour = comp, group = comp)) +
  geom_line(size = 1.2) +
  scale_x_discrete(
    labels = c("-30-","-30","-20","-15","-10","-5","-3","-2","-1","-0.75","-0.50","-0.25","-0.1",
               "0.1","0.25","0.50","0.75","1","2","3","5","10","15","20","30","30+")) +
  xlab("Growth Values (%)") + ylab("Probability (%)") +
  ggtitle(title) +
  labs(colour = "Companies\nCodes")




# The following website contains a guide for ggplot2
# https://r-graphics.org/index.html

# The following site can be used to get exidecimal values for colors
# https://htmlcolorcodes.com/color-picker/





#### PREVIOUS CODE

# n100 = flop
# p100 = top

# pos_percent = c("p0.1","p0.25","p0.50","p0.75","p1","p2","p3",
#                 "p5","p10","p15","p20","p30","p100")
# 
# neg_percent = c("n0.1","n0.25","n0.50","n0.75","n1","n2","n3",
#                 "n5","n10","n15","n20","n30","n100")

# percentages = array()
# j = 1

# calculating probabilities

# for (c in indices) {
#   poss = array()
#   poss_index = 1
#   for (i in neg_percent) {
#     if(value < - as.numeric(gsub("n","", i))) {
#       poss[poss_index] = i
#       poss_index = poss_index + 1
#     }
#   }
#   for (i in pos_percent) {
#     if(value < as.numeric(gsub("p","", i))) {
#       poss[poss_index] = i
#       poss_index = poss_index + 1
#     }
#   }
#   sum_poss = 0
#   for (i in poss) {
#     sum_poss = sum_poss + eval(as.name(paste(i)))[as.numeric(c)]
#   }
#   total_sum = 0
#   for (i in c(neg_percent,pos_percent)) {
#     total_sum = total_sum + eval(as.name(paste(i)))[as.numeric(c)]
#   }
#   percentages[j] = format(round(sum_poss/total_sum*100, 2), nsmall = 2)
#   j = j + 1
# }


#data = data.frame(company_code = companies, probability = percentages)
#ggplot(data, aes(x = reorder(company_code, - as.numeric(gsub("%","",probability))),company_code, y=probability)) + 
#  geom_bar(fill = rgb(0.1,0.4,0.5,0.7), stat = "identity") + xlab("Company Code") + ylab("Probability")




