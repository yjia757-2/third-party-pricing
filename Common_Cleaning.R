library(dplyr)
library(sjmisc)
library(gsubfn)
library(tidyverse)
library(zoo)
library(modelr)
library(splines)
library(hexbin)
library(stringr)
library(reshape2)
library(bpa)
library(stringr)

# Date: Oct 31, 2018
# Updated On: Dec 13, 2018
# Author: Yiran Jia
# Project Name: MD Buyline Price Strategy Analysis
#
version = "c3"
#
# this code is for combining MDB Philips and Siebel, and MDB competitor together. and mark products with rev.x


setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Ketan/MD_Buyline/Data/R_Script/Philips_Price_STwo")
rev_products <- read.csv("c3_Rev_Products.csv", stringsAsFactors = FALSE) %>% select(-X)

# columns they are in common
# Source 
# Qtr
# Modality
# Platform
# Model_Family
# ID
# ID_Name 
# Upgrade
# Demo
# DS
# Philips_Seg1
# Philips_Seg2 
# List_Price
# Quoted_Price
# Discount
# Trade_FMV
# ASP2
# Overall_Discount 
# M_Comment
# M_Trade_In_Incld
# M_Trade_In_Excld
# M_GPO_Disc
# M_Bundle_Purchase
# M_Promotion
# M_University
# M_Part_Multi_Quote
# M_Multi_System_Disc
# M_Negotiated_Disc
# List_Price_Deal
# M_FY_Disc
# M_Alliance_Disc
# M_Final_Negotiated


# make a giant clean datasest that includes shared columns of MDB and Leigh, and merge them 
# into one 
merge_mdb_siebel <- function(dataset1, dataset2, dataset3) {
  # Siebel
  dataset1 <- read.csv(dataset1, stringsAsFactors = FALSE) %>% select(-X) %>% 
    select(Source, Model_Name, Qtr, Modality, Platform, Model_Family, ID, ID_Name,
           List_Price, Quoted_Price,
           Discount, Trade_FMV, ASP2, Overall_Discount, Comments) %>% mutate(M_Trade_In_Incld = NA, M_Trade_In_Excld = NA, M_GPO_Disc = NA, M_Bundle_Purchase = NA, 
                                                                   M_Promotion = NA, M_University = NA, M_Part_Multi_Quote = NA, M_Multi_System_Disc = NA, M_Negotiated_Disc = NA, 
                                                                   M_List_Price_Deal = NA, M_FY_Disc = NA, M_Alliance_Disc = NA)
  #MDB Philips
  dataset2 <- read.csv(dataset2, stringsAsFactors = FALSE) %>% select(-X) %>% 
    select(Source, Model_Name, Qtr, Modality, Platform, Model_Family, ID, ID_Name,
           List_Price, Quoted_Price,
           Discount, Trade_FMV, ASP2, Overall_Discount, Comments, M_Trade_In_Incld, M_Trade_In_Excld, M_GPO_Disc, M_Bundle_Purchase, 
           M_Promotion, M_University, M_Part_Multi_Quote, M_Multi_System_Disc, M_Negotiated_Disc, M_List_Price_Deal, M_FY_Disc, M_Alliance_Disc) %>% distinct()
  # MDB Competitors
  dataset3 <- read.csv(dataset3, stringsAsFactors = FALSE) %>% select(-X) %>% 
    select(Source, Model_Name, Qtr, Modality, Platform, Model_Family, ID, ID_Name,
           List_Price, Quoted_Price,
           Discount, Trade_FMV, ASP2, Overall_Discount, Comments, M_Trade_In_Incld, M_Trade_In_Excld, M_GPO_Disc, M_Bundle_Purchase, 
           M_Promotion, M_University, M_Part_Multi_Quote, M_Multi_System_Disc, M_Negotiated_Disc, M_List_Price_Deal, M_FY_Disc, M_Alliance_Disc) %>% distinct() %>% filter(!is.na(ID_Name))

  common <- rbind(dataset1, dataset2, dataset3)
}
rev_original_common <- merge_mdb_siebel("c3_Rev_Original_Siebel.csv", "c3_Rev_Original_MDB.csv", "c3_Rev_All_Competitors_MDB.csv")


# Be Careful you need to use combined rev_products here. Need to Develope the code here for future use 
for(i in 1:nrow(rev_products)) {
  rev_original_common[grepl(rev_products[i,1], rev_original_common$Model_Name), "Rev_Products"] = 1
} 
rev_original_common[rev_original_common$Source != "Siebel" & 
                    rev_original_common$Source != "MDB-PHILIPS", "Rev_Products"] = 1
rev_original_common[is.na(rev_original_common$Rev_Products), "Rev_Products"] = 0

rev_original_common <- rev_original_common %>% mutate(All_Source = Source) %>% select(All_Source, Source, everything())
rev_original_common[rev_original_common$Source != "MDB-PHILIPS" & rev_original_common$Source != "Siebel", "Source"] <- "MDB-Competitors"

rev_original_common <- rev_original_common %>% mutate(All_Model_Name = Model_Name) %>% select(All_Source, Source, 
                                                                                              All_Model_Name, Model_Name, everything())
rev_original_common[rev_original_common$All_Source == "MDB-PHILIPS", "All_Model_Name"] <- rev_original_common[rev_original_common$All_Source == "MDB-PHILIPS", "ID_Name"]
rev_original_common[rev_original_common$All_Source == "Siebel", "All_Model_Name"] <- rev_original_common[rev_original_common$All_Source == "Siebel", "ID_Name"]

# we need to create a primary key so that we can inner join common & common2 together perfectly. 
rev_original_common <- rev_original_common %>% mutate(Primary_Key = 1:n())

# the result of this file is rev_original_common
write.csv(rev_original_common, paste0(version, "_Rev_Original_Common.csv"))


rev_original_common2 <- cbind(rev_original_common[c(1:19, 30:31)], stack(rev_original_common[20:29]))
colnames(rev_original_common2)[c(22,23)] <- c("Answer", "Factors")
rev_original_common2 <- rev_original_common2 %>% select(-M_Trade_In_Incld, -M_Trade_In_Excld)
# please note that this file combines with MDB philips, Siebel, and compeitotrs. We first redo for the rev version2 on mdb-philips & rev products, 
# export the version on desktop, and then rbind with version 1, and then mannully change the name of both mdb-philps & rev products to "c2". 
# Since Siebel & Competitor file covers all products of modalities, we hardly change them to a different version. Therefore 
# we will combine c2_mdb_philips, v1_Siebel, and v1_competitor in this file and called it c2_common. 
write.csv(rev_original_common2, paste0(version, "_Rev_Original_Common2.csv"))







