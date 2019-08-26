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
setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Ketan/MD_Buyline/Data/R_Script/Philips_Price_STwo")
source("MDB_Cleaning.R")

# Date: Oct 4, 2018
# Updated On: Nov 29, 2018
# Author: Yiran Jia
# Project Name: MD Buyline Price Strategy Analysis
#
version = "v3"
#
# This code is for getting, extracting, and managing competitor informaiton. 

# # since the mdb_priginal is only cover 2018 Q123, I edited the old version and rbind with this new mdb_original. This is just one time use since I don't have any 
# # pervious competitor table. In the future, you only clean the new mdb_original competitior info, and then rbind with any previosu cleaned competiitor report. 
# setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Ketan/MD_Buyline/Data/R_Script/Philips_Price_STwo/MDB_All_Data/2017Q3_Q4_MDB_All_Data")
# old_mdb_original <-read.csv("2017Q3_Q4_MDB_All_Data.csv", stringsAsFactors = FALSE)
# old_mdb_original$Qtr <- as.yearqtr(old_mdb_original$Qtr, format = "%Y Q%q")
# mdb_original <- rbind(mdb_original, old_mdb_original)
setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Ketan/MD_Buyline/Data/R_Script/Philips_Price_STwo")
rev_products <- read.csv("c3_Rev_Products.csv", stringsAsFactors = FALSE) %>% select(-X)

# this is for preparing getting the list of MDB modalitiies of the revlenet philips product we are interested
for (i in 1:nrow(rev_products)){
  each <- mdb_original %>% filter(grepl(rev_products[i,1], Model_Name))
  # Before you grabing all the rev products from rev_original_mdb, 
  # you need to make sure there're products listed in your dataset. otherwise you get 0. 
  if(!exists("rev_original_mdb")) {
    rev_original_mdb <- each
  } else {
    rev_original_mdb <- rbind(rev_original_mdb, each)
  }
}
# from this special form of select_original list, we know what the original modality these equipment were listed in MD Buyline. 
# then I use this unique(list), to find the other competitor products that are listed in the same MDB modalities. 
rev_mdb_modality <- unique(rev_original_mdb$M_Modality)

# get all competitor who are put in the same categories with Philips rev product in MDB
all_competitor <- mdb_original[mdb_original$M_Modality %in% rev_mdb_modality,] 
colnames(all_competitor)[1] <- "Modality"

# # Updated required based on need. extracting unique competitor products for marketing leaders checking. and then use that list MDB_Competitors_Comparison.xlsx as 
# # guidance for you to filling out the "Philips" ID names for these competitor products. In sum, this generate a nice guidnace for you to know which 
# # competitor match with which philips products. Once you update the rev product, you need to REPOPEN it, and generate a new list, and use that to discuss
# # with leaders to help you compare new competitors products. 
# # since we only know a roguh modality informaiton. we need to get mdb modality, vendor name, and model name. print out this list to discuss with marketing leaders.
# # once you come up with rev_product for each competitor. you're good. Then please go back to previous line. continueing exacting priving information of competitors. 
# competitor_list <- all_competitor %>% filter(Upgrade == 0) %>% select(Modality, Vendor, Model_Name) %>% distinct()
# write.csv(competitor_list, "Competitor_List.csv")

# Real step: generate simialr report as Rev_Original_MDB.csv for other competitors 
# this report will includde all competitors on Md Buyline, however please note that marketing leaders are not interested in all of the competitors. Please 
# refer to the competitor guidance for detail. Once generate the big report, you should be able to fill in detail informaiton based on the guidance. The 
# code below is very similar with MDB_Focus_Cleaning.R. However all_competitor doesn't need to be filtered by rev product since everything there is already
# only belong to rev products. 

all_competitor <- all_competitor %>% filter(Vendor != "PHILIPS")

# step5: Expand columns
# what you already have in rev_original_mdb: Vendor, Modality, Model_Name, Qtr, Upgrade, M_Seg1, M_Seg2, List_Price, Quoted_Price, M_Comments, Discount
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
all_competitor <- all_competitor %>% mutate(Source = paste("MDB", Vendor, sep = "-"), Platform = NA, Model_Family = NA, ID = NA, ID_Name = NA, 
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
all_competitor <- unlock_comment(all_competitor, "M_Comments", "Demo", "demo")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_Trade_In_Incld", "Trade in included")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_Trade_In_Excld", "Trade in excluded")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_GPO_Disc", "A group discount applies")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_Bundle_Purchase", "bundled purchase")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_Promotion", "promotional discount")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_University", "university")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_Part_Multi_Quote", "part of a multi-section")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_Multi_System_Disc", "multi-system discount")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_Negotiated_Disc", "negotiated discount")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_List_Price_Deal", "list price")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_FY_Disc", "fiscal year or quarter year-end discount")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "M_Alliance_Disc", "Strategic Alliance")
all_competitor <- unlock_comment(all_competitor, "M_Comments", "DS", "refurbished")

# Re-Arrange the order 
all_competitor <- all_competitor %>% 
  filter(Upgrade == 0, Demo == 0, DS == 0) %>% 
  select(-Vendor, -Upgrade, -Demo, -DS) %>% 
  select(Source, Model_Name, Qtr, Modality, Platform, Model_Family, ID, ID_Name, M_Seg1, M_Seg2, 
         List_Price, Quoted_Price, Discount, Trade_FMV, ASP2, Overall_Discount, everything())


# Similar with Siebel data file. we hardly change the competitor file, because once we pull, we pull all of the products that related with the 
# modality we are interested in. Modality here is slightly different from Siebel, for example, modality Mobile C-Arm & Cath Lab here are both in
# the category of IGT in Siebel. Anyhow, both version of siebel & mdb_competitor should be very stable unless we are doing something new beyong the 
# current research modality. 
write.csv(all_competitor, paste0(version, "_Rev_All_Competitors_MDB.csv"))

# next step: clean mannually and fill out informaiotn (especially ID_Name) with marketing leaders guidance.
# look at Treade-in Included & Excluded to figure out the ASP2 & Overall_Discount
# Then design dataframe for Tableau report. 


















