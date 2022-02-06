library(tidyverse)
library(glmnet)
library(readxl)


data <-read_excel("Cars_Data.xlsx",sheet = "Infinity Data")    # read excel file and label the data as "data"
y <-  data[,17]
x <-  as.matrix(data[,2:16])
cor_mat = cor(x)


perform_bootstrap <- function(data, Steps, sample_size) {
  
  iso_angle <- matrix(0, Steps, 1)
  
  for (i in seq(1, Steps)) {
  samp <- sample(sample_size,length(colnames(data)), replace = T)
  bootstrap_data = data[samp,]
  
  y <-  bootstrap_data[,17]
  x <-  as.matrix(bootstrap_data[,2:16])
  cor_mat = cor(x)
  
  ### eigen vector ####
  
  eigen_data_decomp <-  eigen(cor_mat)
  eigen_values <- eigen_data_decomp$values
  eigen_vector <- eigen_data_decomp$vectors
  
  ego <- eigen_values[eigen_values > 1]
  
  # number of factors to retain
  factors_to_retain <- nrow(as.matrix(ego))					
  eigen_vector_retained<- eigen_vector[,1:factors_to_retain]
  
  eigen_vector_retained_less_0.3<- ifelse(abs(eigen_vector_retained) < 0.3, 0, eigen_vector_retained)
  eigen_vector_retained_positive <- ifelse(eigen_vector_retained_less_0.3 < 0, (-1)*eigen_vector_retained_less_0.3, eigen_vector_retained_less_0.3)		
  
  
  rownames(eigen_vector_retained_positive) <- c("Attractive", "Quiet", "Unreliable", "Poorly Built", "Interesting", "Sporty", "Uncomfortable", "Roomy", "Easy Service", "Prestige", "Common", "Economical", "Successful", "AvantGarde", "Poor Value")
  z <-  x %*% eigen_vector_retained_positive
  
  lm_subset_4z_variables <- lm(bootstrap_data$`Overall Preference` ~ z)
  b1 = as.vector(coef(lm_subset_4z_variables)[2]) #
  b2 = as.vector(coef(lm_subset_4z_variables)[3])
  slope.iso.preference = - b1/b2   
  
  slope.ideal.vector = b2/b1 
  
  angle.iso.preference = atan(slope.iso.preference)*180/pi	
  angle.ideal.vector = atan(slope.ideal.vector)*180/pi
  
  print(angle.ideal.vector)
  print(angle.iso.preference)
  
  iso_angle[i] <- angle.iso.preference
  
  
  }
  
  iso_angle.lower <- round(sort(iso_angle)[25],digits=4)		# 25th value in sorted 
  iso_angle.mean <-round(sort(iso_angle)[500],digits=4)		    # 500th value in sorted  
  iso_angle.upper <- round(sort(iso_angle)[975],digits=4)
  
  print("Lower bound for iso angle =")
  print(iso_angle.lower )
  print("Upper bound for iso angle =")
  print(iso_angle.upper )
  print("Median for iso angle =")
  print(iso_angle.mean )
}



perform_bootstrap(data, 1000, length(row.names(data)))

