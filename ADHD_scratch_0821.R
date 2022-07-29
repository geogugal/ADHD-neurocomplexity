## Scratch script for ADHD ABM project 
setwd("C:/Users/jugal/Dropbox/Research/Neuroscience/ADHD")
library(tidyverse)


# Add new coloumn to prediscane output to normalize 
enzyme_df <- read_csv("Copy of CES1_ADHDME_kids_LIVER_score.csv")
plot(enzyme_df$PRS_1.0)
hist(enzyme_df$PRS_1.0)

#Standardize CES1A1 PRS
enzyme_df$PRS_1.0 <- (enzyme_df$PRS_1.0 - min(enzyme_df$PRS_1.0)) / (max(enzyme_df$PRS_1.0) - min(enzyme_df$PRS_1.0))
summary(enzyme_df)
plot(enzyme_df$PRS_1.0)
hist(enzyme_df$PRS_1.0)

library(stringr)
enzyme_df$ID <- as.numeric(str_sub(enzyme_df$ID1, start = -4))

library(Amelia)
missmap(enzyme_df)

summary(enzyme_df) # 7 new NAs from taking only last 4 digits? 




----
# Depreciated sample below:
# Demonstrate ID - Enzyme level agent input call
# agent will require id input; then based on id will retrive enzyme level from enzyme_df

ID <- setRefClass("id",
                      #Fields indicate attributes this reference class will have
                      fields = list(id = "numeric"), # field for id input
                      
                  
                      #methods indicate what functions the reference class can perform
                      methods = list(
                        
                        #f(x) to hold metabolised products from the liver
                        #superassignments used; D & L_MPH will be updated in global env
                        digest = function(substrate){
                          local_enzyme <- enzyme_df_2 %>% filter(ID2 == id)
                          CES1A1 <<- local_enzyme$CES1A1
                          product <<- substrate * CES1A1
                          substrate <<- substrate - product
                          return(list(substrate,
                                      product,
                                      id,
                                      CES1A1))
                        }
                      )
)

id_0 <- ID$new(id = 0103) ##sting need reinrpretation as numeric
id_0$digest(100)
