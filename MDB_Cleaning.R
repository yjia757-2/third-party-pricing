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
library(stringr)

# Date: Sep 19, 2018
# Updated On: Oct 26, 2018
# Author: Yiran Jia
# Project Name: MD Buyline Price Strategy Analysis
#
# This code is for cleaning and reorganizing MD Buyline data. It only used for general dataset. Once you clean the data, which includes everything 
# you collect from MD buyline, you need to moce to MDB_Focus_Cleaning.R

# SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSStep 1: Download data from MD Buyline 
# Prequesties: download the entire one first and add an column called M_Seg
# download the other segmentations and add an column called M_Seg, filled with the spercific segmentation 
# store the list of files in MDB_All_Data folder. 
# Rbind them together to create mdb_original df with columns Vendor, Upgrade, Model_Name, List_Price, Quoted_Price, Discount, M_Commnet, M_Seg, and M_Modality. 
# delete the duplicates based on columns 1:8
# get each file names from the setwd direct folder. You need to change to the desire folder before running
setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Ketan/MD_Buyline/Data/R_Script/Philips_Price_STwo/MDB_All_Data/2018Q4_2019Q1_MDB_All_Data")
filenames <- list.files(pattern = "*.csv", full.names = TRUE)
filenames <- data.frame(substr(filenames, 3, nchar(filenames)), stringsAsFactors = FALSE)
colnames(filenames) <- "Names"
temp <- unlist(strsplit(filenames$Names, "_"))
temp <- data.frame(matrix(temp, ncol = 2, byrow = TRUE))
filenames <- filenames %>% mutate(M_Modality = temp$X1)

for (i in 1:nrow(filenames)) {
  each <- read.csv(filenames[i,1], stringsAsFactors = FALSE, na.strings=c("", "NA"))
  colnames(each) = each[2, ]
  each = each[c(-1,-2), -1] 
  each <- each %>% select(1:9) %>% mutate(filenames[i,2])
  each <- each %>% filter(!is.na(Vendor))
  each[is.na(each$Upgrade), "Upgrade"] <- "0"
  each[each$Upgrade == "Yes", "Upgrade"] <- "1"
  colnames(each) <- c("Vendor", "Upgrade", "Model_Name", "List_Price", "Quoted_Price", "Discount", "Qtr", "Comments", "M_Seg", "M_Modality")
  each <- each %>% select(M_Modality, everything())
  if(!exists("mdb_original")) {
    mdb_original <- each
  } else {
    mdb_original <- rbind(mdb_original, each)
  }
}
temp_seg <- mdb_original[!is.na(mdb_original$M_Seg),]
temp_uniq <- mdb_original[!duplicated(mdb_original[,1:9]),]
temp_uniq <- temp_uniq %>% select(-M_Seg)
mdb_original <- left_join(temp_uniq, temp_seg, by = c("M_Modality", "Vendor", "Upgrade", "Model_Name", "List_Price", "Quoted_Price", "Discount", "Qtr", "Comments"))  



# SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSStep 2: Format everything in the right way 
# list price, quoted price, and discount are numerics without $ in front 
# quarter to date 
# separate the segmentation by : between the two

clear_money <- function(price) {
  price <- gsub('[$]', '', price)
  price <- as.numeric(gsub(',', '', price))
}
mdb_original$List_Price <- clear_money(mdb_original$List_Price)
mdb_original$Quoted_Price <- clear_money(mdb_original$Quoted_Price)
mdb_original$Discount <- abs((mdb_original$Quoted_Price - mdb_original$List_Price)/ mdb_original$List_Price)
mdb_original$Qtr <- as.yearqtr(mdb_original$Qtr, format = "Q%q %Y")
mdb_original <- mdb_original %>% 
  separate(M_Seg, into = c("M_Seg1", "M_Seg2"), sep = ":")


# the result of this file is mdb_original
# Next step. please open MDB_Focus_Cleaning.R












