library(Hmisc)
library(stringr)
library(dplyr);library(ggplot2)
big_train = read.csv(file = "C:/Users/HP/Documents/GitHub/Big-Mart-Sales-III/Data/Train.csv",
  header = T,sep = ",")
big_test = read.csv(file = "C:/Users/HP/Documents/GitHub/Big-Mart-Sales-III/Data/Test.csv",
  header = T,sep = ",",as.is = c(1))

apply(big_train,2,function(x) sum(is.na(x))) # to check for missing values in training data set

big_comb = bind_rows(big_test,big_train)
apply(big_comb,2,function(x) sum(is.na(x)))
# Item_Weight contains all the missing vales
# Imputing vmedian values

# big_train$Item_Weight[is.na(big_train$Item_Weight)] = 12.860 # Instead of manual or crude imputatuob we will use 
# KNN Imputation

big_comb = kNN(data = big_comb,variable = c("Item_Weight"),impNA = T,trace = T) # We have succesfully imputed the values.

# big_cont = big_comb[,c(1,3,5)]
# pca_cont = prcomp(big_cont,scale. = T,center = T)
# print(pca_cont)
# summary(pca_cont)
# plot(pca_cont,type = "l") # We don't need PCA.

# First we clean the 2 colunn

big_comb[,3] = str_replace(string = big_comb$Item_Fat_Content,
  pattern = "LF",replacement = "Low Fat")
big_comb[,3] = str_replace(string = big_comb$Item_Fat_Content,
  pattern = "low fat",replacement = "Low Fat")
big_comb[,3] = str_replace(string = big_comb$Item_Fat_Content,
  pattern = "Regular",replacement = "reg")

# First we need to convert it into factors

big_comb[,2] = factor(x = big_comb$Item_Fat_Content,
  labels = c(0,1),levels = c("Low Fat","reg"))
big_comb$Outlet_Size = factor(x = big_comb$Outlet_Size,
  labels = c(0,1,2,3),levels = c("Small","Medium","High",""))
big_comb$Outlet_Location_Type = factor(x = big_comb$Outlet_Location_Type,
  labels = c(0,1,2),levels = c("Tier 1","Tier 2","Tier 3"))

big_comb$Item_Identifier = factor(x = big_comb$Item_Identifier,
  labels = c("FD","NC","DR"),levels = c(""))


# Now we will create a new column "Years of operation"

yrs_op = 2017 - big_comb$Outlet_Establishment_Year
big_comb = cbind(yrs_op,big_comb)
big_comb = big_comb[,-9]

# Mean Sales by Outlet_type

big_comb %>%
  group_by(big_comb$Outlet_Type)%>%
  summarise(mean(Item_Weight),mean(Item_Outlet_Sales,na.rm = T))

# As in 879 obsv. visibility is 0,which dosen't make any sense
sum(big_comb$Item_Visibility == 0)

# Removing 0 from Item_visibility we used for loop;
for(i in 1:length(big_comb$Item_Visibility)){
  if(big_comb$Item_Visibility[i] == 0){
    big_comb$Item_Visibility[i] = mean(big_comb$Item_Visibility)
  }else{
    big_comb$Item_Visibility
  }
}

# Creating a broad category of Items:
big_comb$Item_Identifier[str_detect(big_comb$Item_Identifier,pattern = "FD")] = "Food"
big_comb$Item_Identifier[str_detect(big_comb$Item_Identifier,pattern = "NC")] = "Non-Cons"
big_comb$Item_Identifier[str_detect(big_comb$Item_Identifier,pattern = "DR")] = "Drinks"

table(big_comb$Item_Identifier)

# Because there are some Low fat content in NC product type;
# thus it is Non-Edible in nature.
big_comb$Item_Fat_Content[str_detect(big_comb$Item_Identifier,pattern = "Non-Cons")] = "Non-Edible"


