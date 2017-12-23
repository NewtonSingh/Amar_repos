
install.packages("HistData")
library(HistData)
data("ChestSizes")
View(ChestSizes)


sample_chest = function(n){
  mean(sample(ChestSizes$chest,size = n,replace = T))
}



sample_chest1 = function(n){
  mean(sample((ChestSizes$chest - 40.4)/ sqrt(22.66),size = n,replace = T))
}


plot(sapply(1:1000,sample_chest),type = "l", xlab = "No. of chestsizes", ylab = "Sample of chestsizes",col = "Blue")
abline(h = 40.5,col = "Red")

hist(sapply(1:100,sample_chest),freq = F,)
qqnorm(sapply(1:1000,sample_chest1))
qqline(sapply(1:1000,sample_chest1),col = "Red")
