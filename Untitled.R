## Packages and Import of Data
library(arules)
library(tidyverse)
library(plyr)
library(arulesViz)
library(ggplot2)
library(skimr)

setwd("/Users/ben/Documents/Data Science/Assignment/ASDM/TASK2")
RetailData <- read.csv("RetailData.csv", colClasses="factor")
RetailData$InvoiceDate <- format.Date(RetailData$InvoiceDate) 

## Remove rows with missing data
nrow(RetailData)
ncol(RetailData)
RetailData <- na.omit(RetailData)
nrow(RetailData)
RetailData <- RetailData[!grepl("C", RetailData$InvoiceNo),]

skim(RetailData)
RetailData$Quantity <- as.numeric(RetailData$Quantity)
Quan <- subset(RetailData, RetailData$Quantity < 0)

##Exploration
RetailData %>% group_by(CustomerID)
RetailData %>% group_by(InvoiceNo)

clear <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p",
           "q","r","s","t","u","v","w","x","y","z","?","check","CHECK","Dagamed",
           "Crushed","crushed ctn","CRUK Commission", "crushed", "crushed boxes",
           "cracked", "counted", "code mix up? 84930", "check?","came coded as 20713",
           "can't find", "broken", "Breakages", "barcode problem", "Bank Charges",
           "AMAZON","amazon","Amazon","amazon adjust", "AMAZON FEE", "Amazon sold sets",
           "amazon sales", "Amazon Adjustment", "adjust", "adjustment","Adjustment",
           "Adjust bad debt", "add stock to allocate online orders")

RetailData <- !grepl(clear, "", RetailData)
A <- RetailData[3]
B <- table(A)
B
## Select Cols wanted
itemlist <- ddply(RetailData,c("CustomerID","InvoiceDate"),  function(df1)paste(df1$Description, collapse = ","))
write.csv(itemlist,"Retail", quote = FALSE, row.names = TRUE)
tr <- read.transactions('Retail', format = 'basket', sep=',')

## Rules
rules <- apriori(tr, parameter = list(supp=0.005, conf=0.9))
rules <- sort(rules, by='confidence', decreasing = TRUE)
inspect(rules[1:20])
summary(rules)

rules2 <- rules[1:20]

plot(rules2)
plot(rules2, method="graph")

