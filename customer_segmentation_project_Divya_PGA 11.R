###
Project Title : "Customer Segmentation using RFM Analysis"
Done By : Divya K 
Batch : PGA 11
###

###PACKAGE NEEDED
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(rmarkdown)

data<-read.csv("C:\\Users\\divya\\Desktop\\project on R And Python Docx\\customer segmentation.csv")
View(data)
str(data) ###541909 : R, 8 : C
names(data)
head(data)
summary(data)
data <- data %>% mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
                        InvoiceDate=as.Date(InvoiceDate,"%m/%d/%Y %H:%M"),CustomerID=as.factor(CustomerID), 
                        Country=as.factor(Country))

sum(is.na(data))###368039
data <- data %>% mutate(Quantity = replace(Quantity, Quantity<=0, NA),UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))
sum(is.na(data))###381180
data <- data %>% drop_na()
sum(is.na(data))###0
str(data) ### 229253 : R , 8 : C
data <- data %>% mutate(total_price = Quantity*UnitPrice)
data<-data %>% select(-Quantity, -UnitPrice,-StockCode)
View(data)
str(data) ### 6 variables
base_date <- lubridate::as_date('2012-01-01', tz = 'UTC')
##time zone = Coordinated Universal Time 
### In the dataset they provided with the zone to generalize we use UTC
rfm_final_data <- data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(base_date-max(InvoiceDate)),
            frequency =n_distinct(InvoiceNo), monetary= sum(total_price))
summary(rfm_final_data)

