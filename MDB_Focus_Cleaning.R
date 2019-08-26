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
# library(bpa)
library(stringr)
setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Ketan/MD_Buyline/Data/R_Script/Philips_Price_STwo")
source("MDB_Cleaning.R")

# Date: Oct 4, 2018
# Updated On: Dec 6, 2018
# Author: Yiran Jia
# Project Name: MD Buyline Price Strategy Analysis
#
version = "v3"
#
# This code is for cleaning the data that is based on your requirement - 1) vendor 2) Qtr 3) rev.x product
# After getting cleand rev_original_mdb data by MDB_Cleaning.R, you need to select the ones Phil is interested in comparing with. 
# In this script, before you run it, you need to know exactly what vendor, what time period, and what rev product you want for next generation of analysis. 
# At the beginning, this code is making up a list called rev_products, which need to be modify every time you analyze. 
# Then it extract rev product and create necessary columns for final analyze 
# In the end, it generate a big data frame that has every column you need. For the ones are blanks, you need to mannually filled information in. 
#
# SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSStep 3: make a function about what you need. MODIFY NEEDED BELOW
# the vendor is Philips
# choose time period to include
select_original <- mdb_original  %>% filter(Vendor == "PHILIPS") 

# combine wiht past ones since MD Buyline only post three quarters together. 
# I combine them together because I did M_Commnet poorly last time, so I want to redo it. 
# in the future, you shouldn't rbind with any past ones. after you done with this script, you mannully filled information, and then you combine with the past ones. 
# setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Ketan/MD_Buyline/Data/R_Script/Philips_Price_STwo/MDB_All_Data/2017Q3_Q4_MDB_All_Data")
# past_temp <- read.csv("2017Q3_Q4_MDB_All_Philips.csv", stringsAsFactors = FALSE)
# past_temp$Qtr <- as.yearqtr(past_temp$Qtr, format = "%Y Q%q")
# select_original <- rbind(select_original, past_temp)


# rev.x product include. I create rev_products, a reference dataframe that includes col1 is vital product name and col2 what modality this product belong to .
# you need to modify based on your need every time. 
rev_products <- data.frame(Philips_Products = c("Ingenuity",
                                                 "DigitalDiagnost",
                                                 "EasyDiagnost",
                                                 "Mobile Diagnost",
                                                 "Mobile DR",
                                                 "Azurion",
                                                 "Allura",
                                                 "Veradius",
                                                 "Pulsera",
                                                 "Ingenia",
                                                 "Affiniti",
                                                 "CX50",
                                                 "EPIQ",
                                                 "Lumify",
                                                 "Sparq",
                                                 "Big Bore", 
                                                 "ProxiDiagnost", 
                                                 "CombiDiagnost",
                                                 "ClearVue")) %>% mutate(Modality = NA)
rev_products[rev_products$Philips_Products == "Ingenuity" |
               rev_products$Philips_Products =="Big Bore", "Modality"] <- "CT"
rev_products[rev_products$Philips_Products == "DigitalDiagnost" |
               rev_products$Philips_Products == "EasyDiagnost" |
               rev_products$Philips_Products == "Mobile Diagnost" |
               rev_products$Philips_Products == "Mobile DR"|
               rev_products$Philips_Products == "ProxiDiagnost"|
               rev_products$Philips_Products == "CombiDiagnost"
              , "Modality"] <- "DXR"
rev_products[rev_products$Philips_Products == "Azurion" |
               rev_products$Philips_Products == "Allura" |
               rev_products$Philips_Products == "Veradius"|
               rev_products$Philips_Products == "Pulsera", "Modality"] <- "IGT"
rev_products[rev_products$Philips_Products == "Ingenia", "Modality"] <- "MR"
rev_products[rev_products$Philips_Products == "Affiniti" |
               rev_products$Philips_Products == "CX50" |
               rev_products$Philips_Products == "EPIQ" |
               rev_products$Philips_Products == "Lumify" |
               rev_products$Philips_Products == "Sparq"|
               rev_products$Philips_Products == "ClearVue"
              , "Modality"] <- "US"

# you may comment out the above code, and devp your own list based on the unique request


for (i in 1:nrow(rev_products)){
  each <- select_original %>% filter(grepl(rev_products[i,1], Model_Name))
  # Before you grabing all the rev products from rev_original_mdb, 
  # you need to make sure there're products listed in your dataset. otherwise you get 0. 
  if(nrow(each) != 0) {
    each$M_Modality <- rev_products[i,2]
  }
  if(!exists("rev_original_mdb")) {
    rev_original_mdb <- each
  } else {
    rev_original_mdb <- rbind(rev_original_mdb, each)
  }
}
colnames(rev_original_mdb)[1] <- "Modality" 

# step5: Expand columns
# what you already have in rev_original_mdb: Vendor, Modality, Model_Name, Qtr, Upgrade, M_Seg1, M_Seg2, List_Price, Quoted_Price, Comments, Discount
# what you need to expand next: 
# source - 
# Platform (you need to id this mannully seems like)
# Model_Family (you need to id this mannully seems like)
# ID (you need to id this mannully seems like)
# ID_Name (you need to id this mannully seems like)
# DS - 
# Philips_Seg1 (you need to id this mannully seems like)
# Philips_Seg2 (you need to id this mannully seems like)
# Trade_FMV (you need to id this mannully seems like)
# ASP2 (you need to id this mannully seems like)
# Overall_Discount (you need to id this mannully seems like)
rev_original_mdb <- rev_original_mdb %>% mutate(Source = paste("MDB", Vendor, sep = "-"), Platform = NA, Model_Family = NA, ID = NA, ID_Name = NA, 
                                         Trade_FMV = NA, ASP2 = NA, Overall_Discount = NA)

# Demo -
# Trade_Inc -
# Trade_Exc - 
# M_Negotiated_Disc -
# M_Final_Negotiated_Deal -
# M_List_Price_Deal -
# M_FY_Disc -
# M_Alliance_Disc -
# M_GPO_Disc -
# M_Bundle_Purchase - 
# M_Multi_System_Disc - 
# M_Promotion - 
# M_University - 
# Part_Multi_Quote - 

# function to add new columns based on commnet content
unlock_comment <- function(dataset, col_title, col_work, phrase) {
  namevector <- col_work
  dataset[,namevector] <- NA
  change <- function(x) {
    if(grepl(phrase, x[col_title], ignore.case = TRUE)) {
      x[col_work] <- 1
    } else {
      x[col_work] <- 0
    }
  }
  answer <- as.data.frame(apply(dataset, 1, change))
  dataset[col_work] <- answer 
  return(dataset)
}
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "Demo", "demo")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_Trade_In_Incld", "Trade in included")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_Trade_In_Excld", "Trade in excluded")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_GPO_Disc", "A group discount applies")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_Bundle_Purchase", "bundled purchase")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_Promotion", "promotional discount")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_University", "university")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_Part_Multi_Quote", "part of a multi-section")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_Multi_System_Disc", "multi-system discount")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_Negotiated_Disc", "negotiated discount")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_List_Price_Deal", "list price")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_FY_Disc", "fiscal year or quarter year-end discount")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Comments", "M_Alliance_Disc", "Strategic Alliance")
rev_original_mdb <- unlock_comment(rev_original_mdb, "Model_Name", "DS", "Diamond")

# Re-Arrange the order 
rev_original_mdb <- rev_original_mdb %>% 
  filter(Upgrade == 0, Demo == 0, DS == 0) %>% 
  select(-Vendor, -Upgrade, -Demo, -DS) %>% 
  select(Source, Model_Name, Qtr, Modality, Platform, Model_Family, ID, ID_Name, M_Seg1, M_Seg2, 
                       List_Price, Quoted_Price, Discount, Trade_FMV, ASP2, Overall_Discount, everything())

# the result of this file is rev_original_mdb and rev_products
# Next step: fill empty columns mannully while discuss with marketing analysts. 
# If you are doing version 1 above, and planning to rbind with version 1later, you should export this file in desktop as temp and then combine with version1,
# and then rename the file c2 (v1 + v2 for example) in the real folder
write.csv(rev_original_mdb, paste0(version, "_Rev_Originals_MDB.csv"))
# for the use of Common_Cleaning.R, I need to export the current rev product list, which is rev_product  
write.csv(rev_products, paste0(version,"_Rev_Products.csv"))
