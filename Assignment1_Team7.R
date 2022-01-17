setwd('/Users/rctrj/UCD/Winter/BAX 442/Homework/HW1')

############### Assignment-1 BAX 442 ##########################################
## Maria Ahumada, Harsh Harsh, Linus Rupp, Melissa Talgo, Pranjal Srivastasa ##

########### Conjoint Analysis Function ##############
## Inputs: Preferences, Design matrix, Own brand design, competitor A's design , competitor B's  design, Cost per each feature
## Output A: Save your partworths individually in the second tab of the Google Sheet - see "Partworths" 
## Outputs B: Save R output in text file for your team
install.packages('tidyverse')

library(ggplot2)
library(tidyverse)

rm(list = ls())
#### Inputs ####
#read the data
preferences_tv <-read.csv("Preferences_Team7_HW1.csv")

#preview of data
head(preferences_tv)

#design matrix (as given in excel Market Share and Optimal Price)
designmatrix <- matrix(c(1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1500, 2500, 2000), 3, 6)
colnames(designmatrix) <- c("Intercept", "Screen75inch", "Screen85inch", "Resolution", "Sony=1", "Price(low=0;hi=1)")
rownames(designmatrix) <- c("My design", "CompetingBrand1 Sony", "CompetingBrand 2 Sharp")
designmatrix


###### STEP 1 - Run linear regression####
#Regress preference on 4 explanatory variables

Brand <- as.factor(preferences_tv$Brand) #Sharp or Sony
Size <- as.factor(preferences_tv$Size) #65inch, 75inch, or 85inch
Resolution <- as.factor(preferences_tv$Resolution) #1K or 4K
Price <- as.factor(preferences_tv$Price) #numeric $2000 or $2500


create_lm = function(Preferences,Size,Resolution,Brand,Price) {
  linear_reg <- lm(Preferences ~ Size + Resolution + Brand + Price)
  return(linear_reg)
}

#This finds the partworths for each individual in the team
Maria_lm <- create_lm(preferences_tv$Maria,Size,Resolution,Brand,Price)
Harsh_lm <- create_lm(preferences_tv$Harsh,Size,Resolution,Brand,Price)
Linus_lm <- create_lm(preferences_tv$Linus,Size,Resolution,Brand,Price)
Melissa_lm <- create_lm(preferences_tv$Melissa,Size,Resolution,Brand,Price)
Pranjal_lm <- create_lm (preferences_tv$Pranjal,Size,Resolution,Brand,Price)


#This finds Std.Error,tvalue, and p-value for each individual in the team
find_summary_lm <- function(lm){
  summary <- summary(lm)
  sink("Assignemnt1.txt")
  print(round(summary$coefficients[,-1],digits=4))
  sink() 
}


###### STEP 2 - Attribute Importance ####
calculate_attribute_importance <- function(lm){
  #attribute range
  Size_range <- max(lm$coefficients[2],lm$coefficients[3]) - min(lm$coefficients[2],lm$coefficients[3])
  Resolution_range <- max(lm$coefficients[4],0)-min(lm$coefficients[4],0)
  Brand_range <- max(lm$coefficients[5],0)-min(lm$coefficients[5],0)
  Price_range <- max(lm$coefficients[6],0)-min(lm$coefficients[6],0)
  
  #Total Range = Sum of all variables range
  total_attribution_range <- Size_range + Resolution_range + Brand_range + Price_range
  
  #Attribute importance
  Size_importance <- round((Size_range/total_attribution_range)*100,digits=0)
  Resolution_importance <- round((Resolution_range/total_attribution_range)*100,digits=0)
  Brand_importance <- round((Brand_range/total_attribution_range)*100,digits=0)
  Price_importance <- round((Price_range/total_attribution_range)*100,digits=0)

  results <- rbind(Size_importance,Resolution_importance,Brand_importance,Price_importance)
  colnames(results) <- c("Attribution %")
  cbind(results)
  
}

#This finds the attribute % for each individual in the team
calculate_attribute_importance(Maria_lm)
calculate_attribute_importance(Harsh_lm)
calculate_attribute_importance(Linus_lm)
calculate_attribute_importance(Melissa_lm)
calculate_attribute_importance(Pranjal_lm)



###### STEP 3 - Willingness to Pay (WTP) for each feature ####
Price_savings <-max(preferences_tv$Price)- min(preferences_tv$Price)
Price_savings

calculate_wtp <- function(lm){
  #WTP for 75" screen size
  WTP_75inch <- round((lm$coefficients[2]) * Price_savings/abs(lm$coefficients[6]),digits=2)
  WTP_75inch
  
  #WTP for 85" screen size
  WTP_85inch <- round((lm$coefficients[3]) * Price_savings/abs(lm$coefficients[6]),digits=2)
  WTP_85inch
  
  # WTP for 4K resolution
  WTP_4K <- round((lm$coefficients[4]) * Price_savings/abs(lm$coefficients[6]),digits=2)
  WTP_4K
  
  # WTP for Sony brand name
  WTP_Sony <- round((lm$coefficients[5]) * Price_savings/abs(lm$coefficients[6]),digits=2)
  WTP_Sony

  results <- rbind(WTP_75inch,WTP_85inch,WTP_4K,WTP_Sony)
  colnames(results) <- c("Willingness to pay(WTP)")
  cbind(results)
  
}

#This finds the Willingness to pay(WTP) for each individual in the team
calculate_wtp(Maria_lm)
calculate_wtp(Harsh_lm)
calculate_wtp(Linus_lm)
calculate_wtp(Melissa_lm)
calculate_wtp(Pranjal_lm)


###### STEP 4 - Utility, Attractiveness,Market Share ####



calculate_utility <- function(lm){
  intercept_cost <- 1000
  screen75_cost <- 500
  screen85_cost <- 1000
  Resolution_cost <- 250
  Sonybrand_cost <- 250
  
  
  cost_vec <- c(intercept_cost, screen75_cost, screen85_cost, Resolution_cost, Sonybrand_cost)
  
  estimatespartworths_vec <- t(lm$coefficients)[,1:5]
  print(estimatespartworths_vec)
  
  My_design <- as.vector(t(designmatrix["My design",1:5]))
  CompetingBrand1_Sony <- as.vector(t(designmatrix["CompetingBrand1 Sony",1:5]))
  CompetingBrand2_Sharp <- as.vector(t(designmatrix["CompetingBrand 2 Sharp",1:5]))
  
  net_cost <- as.numeric(sum(My_design*cost_vec))

  n <- 10000 ## total number of pieces of TV
  profit <- matrix(, nrow = 1, ncol = 2)
  colnames(profit) <- c("Price", "Profit")
  
  sales <- matrix(, nrow = 1, ncol = 2)
  colnames(sales) <- c("Price", "Sales")
  
  utility_mydesign <- as.numeric(sum(estimatespartworths_vec*My_design) + 
                                   (lm$coefficients[6])*(designmatrix["My design",6]-2000)/(2500-2000))
  
  utility_sony <- as.numeric(sum(estimatespartworths_vec*CompetingBrand1_Sony) + 
                               (lm$coefficients[6])*(designmatrix["CompetingBrand1 Sony",6]-2000)/(2500-2000))
  
  utility_sharp <- as.numeric(sum(estimatespartworths_vec*CompetingBrand2_Sharp) + 
                                (lm$coefficients[6])*(designmatrix["CompetingBrand 2 Sharp",6]-2000)/(2500-2000))
  
  
  my_design_marketShare <- exp(utility_mydesign) /(exp(utility_mydesign) + exp(utility_sony) + exp(utility_sharp))
  my_design_sony <- exp(utility_sony) /(exp(utility_mydesign) + exp(utility_sony) + exp(utility_sharp))
  my_design_sharp <- exp(utility_sharp) /(exp(utility_mydesign) + exp(utility_sony) + exp(utility_sharp))
  
  utility_matrix <- matrix(, nrow = 1, ncol = 3)
  colnames(utility_matrix) <- c("Utility", "Attractiveness", "Market.Share")
  utility_matrix <- rbind(utility_matrix, c(utility_mydesign, exp(utility_mydesign),my_design_marketShare*100))
  utility_matrix <- rbind(utility_matrix, c(utility_sony, exp(utility_sony),my_design_sony*100))
  utility_matrix <- rbind(utility_matrix, c(utility_sharp, exp(utility_sharp),my_design_sharp*100))
  
  utility_matrix <- utility_matrix[-1,]
  
  print(utility_matrix)
  
  
  for (itf in seq(1500, 3000, 10)) {
  
    utility_mydesign <- as.numeric(sum(estimatespartworths_vec*My_design) + 
                                     (lm$coefficients[6])*(itf-2000)/(2500-2000))
    
    utility_sony <- as.numeric(sum(estimatespartworths_vec*CompetingBrand1_Sony) + 
                                 (lm$coefficients[6])*(designmatrix["CompetingBrand1 Sony",6]-2000)/(2500-2000))
    
    utility_sharp <- as.numeric(sum(estimatespartworths_vec*CompetingBrand2_Sharp) + 
                                  (lm$coefficients[6])*(designmatrix["CompetingBrand 2 Sharp",6]-2000)/(2500-2000))
    
    
    my_design_marketShare <- exp(utility_mydesign) /(exp(utility_mydesign) + exp(utility_sony) + exp(utility_sharp))
    my_design_sony <- exp(utility_sony) /(exp(utility_mydesign) + exp(utility_sony) + exp(utility_sharp))
    my_design_sharp <- exp(utility_sharp) /(exp(utility_mydesign) + exp(utility_sony) + exp(utility_sharp))

    profit <- rbind(profit, c(itf, n*my_design_marketShare*(itf-net_cost)))
    sales <- rbind(sales, c(itf, n*my_design_marketShare))
  }  
  
  profit <- as.data.frame(profit)
  sales <- as.data.frame(sales)
  
  pt <- ggplot(data = profit, aes(x = Price, y = Profit)) +
    geom_point() +
    geom_label(data = profit[base::which.max(profit[["Profit"]]),],aes(label = profit[base::which.max(profit[["Profit"]]),"Price"])) 
  
  pt1 <- ggplot(data = sales, aes(x = Price, y = Sales)) +
    geom_point() 
  
  return(list(utility_matrix, pt, pt1))

}


###################################################
######## Printing Maria's Data ####################
###################################################


df <- calculate_utility(Maria_lm)


line1 = "********************************"
line = "Maria's Data"
line2 = "*********************************"

write(line1,file="lm.txt",append=TRUE)
write(line,file="lm.txt",append=TRUE)
write(line2,file="lm.txt",append=TRUE)

sink("lm.txt", append=T)
summary(Maria_lm)
sink()

sink("lm.txt", append=T)
summary(calculate_wtp(Maria_lm))
sink()

line4 = "Utility Matrix"

write(line1,file="lm.txt",append=TRUE)
write(line4,file="lm.txt",append=TRUE)
write(line2,file="lm.txt",append=TRUE)

sink("lm.txt", append=T)
df[[1]]
sink()

##### plots #####
### profit ###
df[[2]]
### sales ###
df[[3]]


###################################################
######## Printing Harsh's Data ####################
###################################################


df <- calculate_utility(Harsh_lm)

line1 = "********************************"
line = "Harsh's Data"
line2 = "*********************************"

write(line1,file="lm.txt",append=TRUE)
write(line,file="lm.txt",append=TRUE)
write(line2,file="lm.txt",append=TRUE)

sink("lm.txt", append=T)
summary(Harsh_lm)
sink()

sink("lm.txt", append=T)
summary(calculate_wtp(Harsh_lm))
sink()

line4 = "Utility Matrix"

write(line1,file="lm.txt",append=TRUE)
write(line4,file="lm.txt",append=TRUE)
write(line2,file="lm.txt",append=TRUE)

sink("lm.txt", append=T)
df[[1]]
sink()

##### plots #####
### profit ###
df[[2]]
### sales ###
df[[3]]




###################################################
######## Printing Pranjal's Data ####################
###################################################


df <- calculate_utility(Pranjal_lm)

line1 = "********************************"
line = "Pranjal's Data"
line2 = "*********************************"

write(line1,file="lm.txt",append=TRUE)
write(line,file="lm.txt",append=TRUE)
write(line2,file="lm.txt",append=TRUE)

sink("lm.txt", append=T)
summary(Pranjal_lm)
sink()

sink("lm.txt", append=T)
summary(calculate_wtp(Pranjal_lm))
sink()

line4 = "Utility Matrix"

write(line1,file="lm.txt",append=TRUE)
write(line4,file="lm.txt",append=TRUE)
write(line2,file="lm.txt",append=TRUE)

sink("lm.txt", append=T)
df[[1]]
sink()

##### plots #####
### profit ###
df[[2]]
### sales ###
df[[3]]




###################################################
######## Printing Melissa's Data ####################
###################################################


df <- calculate_utility(Melissa_lm)

line1 = "********************************"
line = "Melissa's Data"
line2 = "*********************************"

write(line1,file="lm.txt",append=TRUE)
write(line,file="lm.txt",append=TRUE)
write(line2,file="lm.txt",append=TRUE)

sink("lm.txt", append=T)
summary(Melissa_lm)
sink()

sink("lm.txt", append=T)
summary(calculate_wtp(Melissa_lm))
sink()

line4 = "Utility Matrix"

write(line1,file="lm.txt",append=TRUE)
write(line4,file="lm.txt",append=TRUE)
write(line2,file="lm.txt",append=TRUE)

sink("lm.txt", append=T)
df[[1]]
sink()

##### plots #####
### profit ###
df[[2]]
### sales ###
df[[3]]




###################################################
######## Printing Linus's Data ####################
###################################################


df <- calculate_utility(Linus_lm)

line1 = "********************************"
line = "Linus's Data"
line2 = "*********************************"

write(line1,file="lm.txt",append=TRUE)
write(line,file="lm.txt",append=TRUE)
write(line2,file="lm.txt",append=TRUE)

sink("lm.txt", append=T)
summary(Linus_lm)
sink()

sink("lm.txt", append=T)
summary(calculate_wtp(Linus_lm))
sink()

line4 = "Utility Matrix"

write(line1,file="lm.txt",append=TRUE)
write(line4,file="lm.txt",append=TRUE)
write(line2,file="lm.txt",append=TRUE)

sink("lm.txt", append=T)
df[[1]]
sink()

##### plots #####
### profit ###
df[[2]]
### sales ###
df[[3]]



