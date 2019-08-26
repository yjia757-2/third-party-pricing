library(dplyr)
library(sjmisc)
library(gsubfn)
library(tidyverse)
library(zoo)
library(ggplot2)
library(modelr)
library(splines)
library(hexbin)
library(stringr)
library(reshape2)
library(bpa)
library(stringr)

# Date: Aug 24, 2018
# Updated On: Jan 28, 2019
# Author: Yiran Jia
# Project Name: MD Buyline Price Strategy Analysis
#
# This code is for cleaning the data downloaded from Siebel
version= "v3"

# PART I 
# basic clean 
# you need to chose the right folder. different period stored in different folders
clean_siebel <- function (file_name, tech) {
  setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Ketan/MD_Buyline/Data/R_Script/Philips_Price_STwo/Siebel_All_Data/2019Q1_Siebel_All_Data")
  dataset <- read.csv(file_name, stringsAsFactors = FALSE)
  colnames(dataset)[colnames(dataset)=="SAP.Submit.Date"] <- "Qtr"
  colnames(dataset)[colnames(dataset)=="Product.Description"] <- "Model_Name"
  colnames(dataset)[colnames(dataset)=="Pricing...System.List"] <- "List_Price" 
  colnames(dataset)[colnames(dataset)=="Pricing...Order.Contract.Amount"] <- "Quoted_Price"
  colnames(dataset)[colnames(dataset)=="Pricing...Trade.In.FMV"] <- "Trade_FMV"
  colnames(dataset)[colnames(dataset)=="Order.Solution.Row.ID"] <- "Order_Solution_Row_ID"
  colnames(dataset)[colnames(dataset)=="Part.Number"] <- "ID"
  colnames(dataset)[colnames(dataset)=="Account.Name"] <- "Account_Name"
  colnames(dataset)[colnames(dataset)=="Quote.Number"] <- "Quote_Number"
  colnames(dataset)[colnames(dataset)=="Solution.Name"] <- "Solution_Name"
  colnames(dataset)[colnames(dataset)=="SAP.Order.Number" ] <- "SAP_Order_Number" 
  colnames(dataset)[colnames(dataset)=="Part.Number" ] <- "Part_Number" 
  colnames(dataset)[colnames(dataset)=="Opportunity.Name" ] <- "Opportunity_Name" 
  colnames(dataset)[colnames(dataset)=="Quote.Name" ] <- "Quote_Name" 
  
  dataset <- dataset %>% mutate(Source = "Siebel",
                                Modality = tech,
                                Platform = NA,
                                Model_Family = NA,
                                ID_Name = NA,
                                Discount = abs((Quoted_Price - List_Price) / List_Price),
                                ASP2 = Quoted_Price + Trade_FMV,
                                Overall_Discount = abs((Quoted_Price + Trade_FMV - List_Price) / List_Price))
  
  dataset <- dataset %>% select(Source, Model_Name, Qtr, Modality, Platform, Model_Family, ID, ID_Name, Product, MAG, Opportunity_Name, Quote_Name, Solution_Name,  
                                List_Price, Quoted_Price, Discount, Trade_FMV, ASP2
                                , Overall_Discount, everything())
  dataset$Qtr <- as.yearqtr(dataset$Qtr, format = "%m/%d/%Y")
  # next time remember to clean out the veterinary machines: account: delete the ones with "animals"
  write.csv(dataset, file_name) # export. reopen the code after mannuel fill the variables columns.  
}

clean_siebel("Siebel_CT.csv", "CT")
clean_siebel("Siebel_DXR.csv", "DXR")
clean_siebel("Siebel_IGT.csv", "IGT")
clean_siebel("Siebel_MR.csv", "MR")
clean_siebel("Siebel_US.csv", "US")

# PART II 
# after you export the files. you need to mannuelly input necessary information. Then come back and delete useless variables, and 
# combine the neutral variables together in commonet, and then rbind for all five modalities. 
# make sure to setwd first 
file_names <- c("Siebel_CT.csv","Siebel_DXR.csv","Siebel_IGT.csv","Siebel_MR.csv","Siebel_US.csv")
for (i in 1:length(file_names)){
  dataset <- read.csv(file_names[i], stringsAsFactors = FALSE) %>% select(-X, -Product, -MAG, -Opportunity_Name, -Quote_Name) %>% 
  select(Source, Model_Name, Qtr, Modality, Platform, Model_Family, ID, ID_Name, List_Price, Quoted_Price, Discount, Trade_FMV,
         ASP2, Overall_Discount, everything())
  # combing the column into one column called Comments. 
  dataset[15:20] <- Map(paste, names(dataset[15:20]), dataset[15:20], sep = ':') # i.e. Solution_Name: EPIQ 7C instead of EPIQ 7C
  cols <- colnames(dataset[15:20]) # columns that we try to colllaspe 
  dataset$Comments <- apply( dataset[ , cols ] , 1 , paste , collapse = "/ " ) # create a new column `Comments` 
  dataset <- dataset[, !( names(dataset) %in% cols)] # remove the old columns
  if(!exists("rev_original_siebel")){
    rev_original_siebel <- dataset
  } else {
    rev_original_siebel <- rbind(rev_original_siebel, dataset)
  }
}

write.csv(rev_original_siebel, paste0(version, "_Rev_Original_Siebel.csv"))

# ready to be used in Common_Cleaning.R, and then Tableau Report

