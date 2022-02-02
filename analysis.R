#analysis

#probs

#zero = array()

p0.1 = array()
p0.25 = array()
p0.50 = array()
p0.75 = array()
p1 = array()
p2 = array()
p3 = array()
p5 = array()
p10 = array()
p15 = array()
p20 = array()
p30 = array()
top = array()

n0.1 = array()
n0.25 = array()
n0.50 = array()
n0.75 = array()
n1 = array()
n2 = array()
n3 = array()
n5 = array()
n10 = array()
n15 = array()
n20 = array()
n30 = array()
flop = array()


# Sobstitute the upper limit of the for loop below with the following line of code:
length(stocks[,1])

for (i in 1:length(stocks[,1])) {
  
  p0.1[i] = 0
  p0.25[i] = 0
  p0.50[i] = 0
  p0.75[i] = 0
  p1[i] = 0
  p2[i] = 0
  p3[i] = 0
  p5[i] = 0
  p10[i] = 0
  p15[i] = 0
  p20[i] = 0
  p30[i] = 0
  top[i] = 0
  
  n0.1[i] = 0
  n0.25[i] = 0
  n0.50[i] = 0
  n0.75[i] = 0
  n1[i] = 0
  n2[i] = 0
  n3[i] = 0
  n5[i] = 0
  n10[i] = 0
  n15[i] = 0
  n20[i] = 0
  n30[i] = 0
  flop[i] = 0
  
  for (j in 2:length(stocks[1,])) {
    
    #zero[i] = 0
    
    if(stocks[i,j] != "null" & stocks[i,j-1] != "null") {
      
    
    if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) > 0 ) {
      
      if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.1 ) { p0.1[i] = p0.1[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.25 ) { p0.25[i] = p0.25[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.50 ) { p0.50[i] = p0.50[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.75 ) { p0.75[i] = p0.75[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 1 ) { p1[i] = p1[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 2 ) { p2[i] = p2[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 3 ) { p3[i] = p3[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 5 ) { p5[i] = p5[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 10 ) { p10[i] = p10[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 15 ) { p15[i] = p15[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 20 ) { p20[i] = p20[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 30 ) { p30[i] = p30[i] + 1 }
      else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) > 30 ) { top[i] = top[i] + 1 }
      
    } else if(((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0 ) {
      
      if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.1 ) { n0.1[i] = n0.1[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.25 ) { n0.25[i] = n0.25[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.50 ) { n0.50[i] = n0.50[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 0.75 ) { n0.75[i] = n0.75[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 1 ) { n1[i] = n1[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 2 ) { n2[i] = n2[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 3 ) { n3[i] = n3[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 5 ) { n5[i] = n5[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 10 ) { n10[i] = n10[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 15 ) { n15[i] = n15[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 20 ) { n20[i] = n20[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) < 30 ) { n30[i] = n30[i] + 1 }
      else if( abs((as.numeric(stocks[i,j]) - as.numeric(stocks[i,j-1]))/as.numeric(stocks[i,j-1]) * 100) > 30 ) { flop[i] = flop[i] + 1 }
    }
    } 
  } # closes the internal for
}

#putting all the arrays in one big table and saving it

table_arrays = data.frame(flop,n30,n20,n15,n10,n5,n3,n2,n1,n0.75,n0.50,n0.25,n0.1,
                          p0.1,p0.25,p0.50,p0.75,p1,p2,p3,p5,p10,p15,p20,p30,top)

#save(table_arrays, file = "table_arrays.RData")



# The algorithm is not super precise since a number lower than 0.25 should not count for the vector p0.25, but should count for p0.1 instead. Not even sure of this...
