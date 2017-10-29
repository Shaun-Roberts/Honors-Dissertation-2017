#Load the dataset that you wish to check for normality:
#i.e:
load(paste("faux.magnolia.high_100_0.01_0.01"))

for(j in c(5:7,9:13)) { 
  print(paste("j =", j))
  for(i in 1:50){
    tryCatch(qqnorm(unlist(record[[j]][[i]])), error = function(e) plot(1))
    print(paste("i =", i))
    Sys.sleep(0.01)
  }
}
