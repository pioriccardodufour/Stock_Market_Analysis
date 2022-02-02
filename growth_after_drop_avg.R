# What is the probability of a generic French stock price to growth or drop of 
# a certain amount after a DROP occurring, whose value is bigger than a specified amount?

#Define the daily drop value you are interested in.
#Based on the value you put, the algorithm will analyse only the data related to periods
#in which a drop of the specified amount occurred.

value_drop = 20

# load stocks
load("~/R/RStudio/Finance Project/stocks.RData")

##########################
d1n0.1 = array()
d1n0.25 = array()
d1n0.50 = array()
d1n0.75 = array()
d1n1 = array()
d1n2 = array()
d1n3 = array()
d1n5 = array()
d1n10 = array()
d1n15 = array()
d1n20 = array()
d1n30 = array()
d1flop = array()

d1p0.1 = array()
d1p0.25 = array()
d1p0.50 = array()
d1p0.75 = array()
d1p1 = array()
d1p2 = array()
d1p3 = array()
d1p5 = array()
d1p10 = array()
d1p15 = array()
d1p20 = array()
d1p30 = array()
d1top = array()

d3n0.1 = array()
d3n0.25 = array()
d3n0.50 = array()
d3n0.75 = array()
d3n1 = array()
d3n2 = array()
d3n3 = array()
d3n5 = array()
d3n10 = array()
d3n15 = array()
d3n20 = array()
d3n30 = array()
d3flop = array()

d3p0.1 = array()
d3p0.25 = array()
d3p0.50 = array()
d3p0.75 = array()
d3p1 = array()
d3p2 = array()
d3p3 = array()
d3p5 = array()
d3p10 = array()
d3p15 = array()
d3p20 = array()
d3p30 = array()
d3top = array()

d7n0.1 = array()
d7n0.25 = array()
d7n0.50 = array()
d7n0.75 = array()
d7n1 = array()
d7n2 = array()
d7n3 = array()
d7n5 = array()
d7n10 = array()
d7n15 = array()
d7n20 = array()
d7n30 = array()
d7flop = array()

d7p0.1 = array()
d7p0.25 = array()
d7p0.50 = array()
d7p0.75 = array()
d7p1 = array()
d7p2 = array()
d7p3 = array()
d7p5 = array()
d7p10 = array()
d7p15 = array()
d7p20 = array()
d7p30 = array()
d7top = array()

for (i in 1:length(stocks[,1])) {

  d1n0.1[i] = 0
  d1n0.25[i] = 0
  d1n0.50[i] = 0
  d1n0.75[i] = 0
  d1n1[i] = 0
  d1n2[i] = 0
  d1n3[i] = 0
  d1n5[i] = 0
  d1n10[i] = 0
  d1n15[i] = 0
  d1n20[i] = 0
  d1n30[i] = 0
  d1flop[i] = 0
  
  d1p0.1[i] = 0
  d1p0.25[i] = 0
  d1p0.50[i] = 0
  d1p0.75[i] = 0
  d1p1[i] = 0
  d1p2[i] = 0
  d1p3[i] = 0
  d1p5[i] = 0
  d1p10[i] = 0
  d1p15[i] = 0
  d1p20[i] = 0
  d1p30[i] = 0
  d1top[i] = 0
  
  d3n0.1[i] = 0
  d3n0.25[i] = 0
  d3n0.50[i] = 0
  d3n0.75[i] = 0
  d3n1[i] = 0
  d3n2[i] = 0
  d3n3[i] = 0
  d3n5[i] = 0
  d3n10[i] = 0
  d3n15[i] = 0
  d3n20[i] = 0
  d3n30[i] = 0
  d3flop[i] = 0
  
  d3p0.1[i] = 0
  d3p0.25[i] = 0
  d3p0.50[i] = 0
  d3p0.75[i] = 0
  d3p1[i] = 0
  d3p2[i] = 0
  d3p3[i] = 0
  d3p5[i] = 0
  d3p10[i] = 0
  d3p15[i] = 0
  d3p20[i] = 0
  d3p30[i] = 0
  d3top[i] = 0
  
  d7n0.1[i] = 0
  d7n0.25[i] = 0
  d7n0.50[i] = 0
  d7n0.75[i] = 0
  d7n1[i] = 0
  d7n2[i] = 0
  d7n3[i] = 0
  d7n5[i] = 0
  d7n10[i] = 0
  d7n15[i] = 0
  d7n20[i] = 0
  d7n30[i] = 0
  d7flop[i] = 0
  
  d7p0.1[i] = 0
  d7p0.25[i] = 0
  d7p0.50[i] = 0
  d7p0.75[i] = 0
  d7p1[i] = 0
  d7p2[i] = 0
  d7p3[i] = 0
  d7p5[i] = 0
  d7p10[i] = 0
  d7p15[i] = 0
  d7p20[i] = 0
  d7p30[i] = 0
  d7top[i] = 0
  
  for (j in 3:length(stocks[1,])) { # internal for  loop
    
    if(stocks[i,j] != "null" & stocks[i,j-1] != "null" & stocks[i,j-2] != "null") {
      
      #I consider storing the growth value only if: 
      #1: the drop value is lower than "value_drop"
      #2: the stock value in the day of the drop is lower than the one of both the previous day and following day
      
      if(((as.numeric(stocks[i,j-1]) - as.numeric(stocks[i,j-2]))/as.numeric(stocks[i,j-2]) * 100) < - value_drop & as.numeric(stocks[i,j-1]) < as.numeric(stocks[i,j-2]) ) {   #& as.numeric(stocks[i,j-1]) < as.numeric(stocks[i,j])  
        
        if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) > 0 ) {
          if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.1 ) { d1p0.1[i] = d1p0.1[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.25 ) { d1p0.25[i] = d1p0.25[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.50 ) { d1p0.50[i] = d1p0.50[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.75 ) { d1p0.75[i] = d1p0.75[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 1 ) { d1p1[i] = d1p1[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 2 ) { d1p2[i] = d1p2[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 3 ) { d1p3[i] = d1p3[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 5 ) { d1p5[i] = d1p5[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 10 ) { d1p10[i] = d1p10[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 15 ) { d1p15[i] = d1p15[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 20 ) { d1p20[i] = d1p20[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 30 ) { d1p30[i] = d1p30[i] + 1 }
          else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) > 30 ) { d1top[i] = d1top[i] + 1 }
        }
        else  if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0 ) {
          if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.1 ) { d1n0.1[i] = d1n0.1[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.25 ) { d1n0.25[i] = d1n0.25[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.50 ) { d1n0.50[i] = d1n0.50[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.75 ) { d1n0.75[i] = d1n0.75[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 1 ) { d1n1[i] = d1n1[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 2 ) { d1n2[i] = d1n2[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 3 ) { d1n3[i] = d1n3[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 5 ) { d1n5[i] = d1n5[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 10 ) { d1n10[i] = d1n10[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 15 ) { d1n15[i] = d1n15[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 20 ) { d1n20[i] = d1n20[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 30 ) { d1n30[i] = d1n30[i] + 1 }
          else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) > 30 ) { d1flop[i] = d1flop[i] + 1 }
        }
        
        if(j > 4) {
          if(stocks[i,j-2] != "null" & stocks[i,j-3] != "null") {
            if(((as.numeric(stocks[i,j-2]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) > 0 ) {      #or maybe i,i-1           
              if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 0.1 ) { d3p0.1[i] = d3p0.1[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 0.25 ) { d3p0.25[i] = d3p0.25[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 0.50 ) { d3p0.50[i] = d3p0.50[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 0.75 ) { d3p0.75[i] = d3p0.75[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 1 ) { d3p1[i] = d3p1[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 2 ) { d3p2[i] = d3p2[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 3 ) { d3p3[i] = d3p3[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 5 ) { d3p5[i] = d3p5[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 10 ) { d3p10[i] = d3p10[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 15 ) { d3p15[i] = d3p15[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 20 ) { d3p20[i] = d3p20[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 30 ) { d3p30[i] = d3p30[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) > 30 ) { d3top[i] = d3top[i] + 1 }
            }
            else if(((as.numeric(stocks[i,j-2]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 0 ) {      #or maybe i,i-1           
              if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 0.1 ) { d3n0.1[i] = d3n0.1[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 0.25 ) { d3n0.25[i] = d3n0.25[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 0.50 ) { d3n0.50[i] = d3n0.50[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 0.75 ) { d3n0.75[i] = d3n0.75[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 1 ) { d3n1[i] = d3n1[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 2 ) { d3n2[i] = d3n2[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 3 ) { d3n3[i] = d3n3[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 5 ) { d3n5[i] = d3n5[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 10 ) { d3n10[i] = d3n10[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 15 ) { d3n15[i] = d3n15[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 20 ) { d3n20[i] = d3n20[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) < 30 ) { d3n30[i] = d3n30[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-3]))/as.numeric(stocks[i,j-3]) * 100) > 30 ) { d3flop[i] = d3flop[i] + 1 }
            }
          }
          
        }
        
        if(j > 8) {
          if(stocks[i,j-6] != "null" & stocks[i,j-7] != "null") {
            if(((as.numeric(stocks[i,j-6]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) > 0 ) {      #or maybe i,i-1
              if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 0.1 ) { d7p0.1[i] = d7p0.1[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 0.25 ) { d7p0.25[i] = d7p0.25[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 0.50 ) { d7p0.50[i] = d7p0.50[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 0.75 ) { d7p0.75[i] = d7p0.75[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 1 ) { d7p1[i] = d7p1[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 2 ) { d7p2[i] = d7p2[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 3 ) { d7p3[i] = d7p3[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 5 ) { d7p5[i] = d7p5[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 10 ) { d7p10[i] = d7p10[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 15 ) { d7p15[i] = d7p15[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 20 ) { d7p20[i] = d7p20[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 30 ) { d7p30[i] = d7p30[i] + 1 }
              else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) > 30 ) { d7top[i] = d7top[i] + 1 }
            }
            else if(((as.numeric(stocks[i,j-6]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 0 ) {      #or maybe i,i-1
              if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 0.1 ) { d7n0.1[i] = d7n0.1[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 0.25 ) { d7n0.25[i] = d7n0.25[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 0.50 ) { d7n0.50[i] = d7n0.50[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 0.75 ) { d7n0.75[i] = d7n0.75[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 1 ) { d7n1[i] = d7n1[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 2 ) { d7n2[i] = d7n2[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 3 ) { d7n3[i] = d7n3[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 5 ) { d7n5[i] = d7n5[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 10 ) { d7n10[i] = d7n10[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 15 ) { d7n15[i] = d7n15[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 20 ) { d7n20[i] = d7n20[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) < 30 ) { d7n30[i] = d7n30[i] + 1 }
              else if(abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-7]))/as.numeric(stocks[i,j-7]) * 100) > 30 ) { d7flop[i] = d7flop[i] + 1 }
            }
          }
          
        }
        
        
      } 
    } 
  } # closes the internal for loop
}

d1n0.1 = sum(d1n0.1)
d1n0.25 = sum(d1n0.25)
d1n0.50 = sum(d1n0.50)
d1n0.75 = sum(d1n0.75)
d1n1 = sum(d1n1)
d1n2 = sum(d1n2)
d1n3 = sum(d1n3)
d1n5 = sum(d1n5)
d1n10 = sum(d1n10)
d1n15 = sum(d1n15)
d1n20 = sum(d1n20)
d1n30 = sum(d1n30)
d1flop = sum(d1flop)

d1p0.1 = sum(d1p0.1)
d1p0.25 = sum(d1p0.25)
d1p0.50 = sum(d1p0.50)
d1p0.75 = sum(d1p0.75)
d1p1 = sum(d1p1)
d1p2 = sum(d1p2)
d1p3 = sum(d1p3)
d1p5 = sum(d1p5)
d1p10 = sum(d1p10)
d1p15 = sum(d1p15)
d1p20 = sum(d1p20)
d1p30 = sum(d1p30)
d1top = sum(d1top)

d3n0.1 = sum(d3n0.1)
d3n0.25 = sum(d3n0.25)
d3n0.50 = sum(d3n0.50)
d3n0.75 = sum(d3n0.75)
d3n1 = sum(d3n1)
d3n2 = sum(d3n2)
d3n3 = sum(d3n3)
d3n5 = sum(d3n5)
d3n10 = sum(d3n10)
d3n15 = sum(d3n15)
d3n20 = sum(d3n20)
d3n30 = sum(d3n30)
d3flop = sum(d3flop)

d3p0.1 = sum(d3p0.1)
d3p0.25 = sum(d3p0.25)
d3p0.50 = sum(d3p0.50)
d3p0.75 = sum(d3p0.75)
d3p1 = sum(d3p1)
d3p2 = sum(d3p2)
d3p3 = sum(d3p3)
d3p5 = sum(d3p5)
d3p10 = sum(d3p10)
d3p15 = sum(d3p15)
d3p20 = sum(d3p20)
d3p30 = sum(d3p30)
d3top = sum(d3top)

d7n0.1 = sum(d7n0.1)
d7n0.25 = sum(d7n0.25)
d7n0.50 = sum(d7n0.50)
d7n0.75 = sum(d7n0.75)
d7n1 = sum(d7n1)
d7n2 = sum(d7n2)
d7n3 = sum(d7n3)
d7n5 = sum(d7n5)
d7n10 = sum(d7n10)
d7n15 = sum(d7n15)
d7n20 = sum(d7n20)
d7n30 = sum(d7n30)
d7flop = sum(d7flop)

d7p0.1 = sum(d7p0.1)
d7p0.25 = sum(d7p0.25)
d7p0.50 = sum(d7p0.50)
d7p0.75 = sum(d7p0.75)
d7p1 = sum(d7p1)
d7p2 = sum(d7p2)
d7p3 = sum(d7p3)
d7p5 = sum(d7p5)
d7p10 = sum(d7p10)
d7p15 = sum(d7p15)
d7p20 = sum(d7p20)
d7p30 = sum(d7p30)
d7top = sum(d7top)

d1_drops_growths = c(d1flop,d1n30,d1n20,d1n15,d1n10,d1n5,d1n3,d1n2,d1n1,d1n0.75,d1n0.50,d1n0.25,d1n0.1,
                              d1p0.1,d1p0.25,d1p0.50,d1p0.75,d1p1,d1p2,d1p3,d1p5,d1p10,d1p15,d1p20,d1p30,d1top)
d3_drops_growths = c(d3flop,d3n30,d3n20,d3n15,d3n10,d3n5,d3n3,d3n2,d3n1,d3n0.75,d3n0.50,d3n0.25,d3n0.1,
                              d3p0.1,d3p0.25,d3p0.50,d3p0.75,d3p1,d3p2,d3p3,d3p5,d3p10,d3p15,d3p20,d3p30,d3top)
d7_drops_growths = c(d7flop,d7n30,d7n20,d7n15,d7n10,d7n5,d7n3,d7n2,d7n1,d7n0.75,d7n0.50,d7n0.25,d7n0.1,
                              d7p0.1,d7p0.25,d7p0.50,d7p0.75,d7p1,d7p2,d7p3,d7p5,d7p10,d7p15,d7p20,d7p30,d7top)

j = 1
growth_prob1 = array()
drop_cases = sum(d1_drops_growths)
for (i in d1_drops_growths) {
  growth_prob1[j] = (i/drop_cases)*100
  j = j + 1
}
j = 1
growth_prob3 = array()
drop_cases = sum(d3_drops_growths)
for (i in d3_drops_growths) {
  growth_prob3[j] = (i/drop_cases)*100
  j = j + 1
}
j = 1
growth_prob7 = array()
drop_cases = sum(d7_drops_growths)
for (i in d7_drops_growths) {
  growth_prob7[j] = (i/drop_cases)*100
  j = j + 1
}

library(ggplot2)

gad1 = data.frame(value = c("-30-","-30","-20","-15","-10","-5","-3","-2","-1","-0.75","-0.50","-0.25","-0.1",
                 "0.1","0.25","0.50","0.75","1","2","3","5","10","15","20","30","30+"),
                 prob = growth_prob1, days_after_drop = rep(1,26))
gad3 = data.frame(value = c("-30-","-30","-20","-15","-10","-5","-3","-2","-1","-0.75","-0.50","-0.25","-0.1",
                 "0.1","0.25","0.50","0.75","1","2","3","5","10","15","20","30","30+"),
                  prob = growth_prob3, days_after_drop = rep(3,26))
gad7 = data.frame(value = c("-30-","-30","-20","-15","-10","-5","-3","-2","-1","-0.75","-0.50","-0.25","-0.1",
                 "0.1","0.25","0.50","0.75","1","2","3","5","10","15","20","30","30+"),
                  prob = growth_prob7, days_after_drop = rep(7,26))
gad = rbind(gad1,gad3,gad7)

remove(gad1)
remove(gad3)
remove(gad7)

title = paste("Probability of the average french company stocks price growing of a certain value, 1,3 and 7 days after a daily drop of ", value_drop,"%")

ggplot(data = gad, aes(x = factor(value), y = prob, fill = factor(days_after_drop))) + 
  geom_col(position = "dodge", width = 0.6) + #,group = 3
  #geom_text(aes(label = prob), vjust = -0.3) + # it works but i need to reduce both length and dimensions of numbers
  scale_fill_manual(values = c("#F17F67", "#79D19A", "#78BFF6")) +
  scale_x_discrete(limits = c("-30-","-30","-20","-15","-10","-5","-3","-2","-1","-0.75","-0.50","-0.25","-0.1",
                         "0.1","0.25","0.50","0.75","1","2","3","5","10","15","20","30","30+")) +
  xlab("Growth Values (%)") + ylab("Probability (%)") +
  ggtitle(title) +
  labs(fill = "Number\nof days")
  



################
# How Much The Stock Market Move On Average A Day. From 1999 – 2019, the stock market as 
# defined by the S&P 500 moves on average -1% and +1% a day, for 70% of the days.
# https://www.financialsamurai.com/how-much-does-the-stock-market-move-on-average-a-day/

# The following website contains a guide for ggplot2
# https://r-graphics.org/index.html

# The following site can be used to get exidecimal values for colors
# https://htmlcolorcodes.com/color-picker/


# The algorithm is not perfect since a number lower than 0.25 should not count for the vector d1p0.25, but should count for p0.1 instead. Not even sure of this...

