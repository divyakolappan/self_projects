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

data<-read.csv("C:\\Users\\divya\\Desktop\\imarticus_project_folder\\customer segmentation.csv")
View(data)
str(data) ###541909 : R, 8 : C
names(data)
head(data)
## checking for outliers
boxplot(data)
boxplot(data$Quantity, plot=FALSE)$out
outliers <- boxplot(data$Quantity, plot=FALSE)$out
outlier_uni <- boxplot(data$UnitPrice,plot = FALSE)$out

data<- data[-which(data$Quantity %in% outliers),]
data <- data[-which(data$UnitPrice %in% outlier_uni),]
y <- boxplot(data)

summary(data)
sum(is.na(data))
data <- data %>% mutate(Quantity = replace(Quantity, Quantity<=0, NA),UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))
sum(is.na(data))
data<- data %>% drop_na()
data<- data %>% mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
                        CustomerID=as.factor(CustomerID), 
                        Country=as.factor(Country))
sum(is.na(data))
data$InvoiceDate= as.Date(data$InvoiceDate, "%m-%d-%Y %H:%M")
data<- data %>% drop_na()
View(data)
str(data)
library(lubridate)
data_cust<- filter(data_full,yearmonth != 201012 & yearmonth != 201112)
data$year_of_purchase = as.numeric(format(data$InvoiceDate, "%Y"))
View(data)
data$month_of_purchase = as.numeric(format(data$InvoiceDate, "%m"))
###for grouping 
data$yearmonth <- paste(data$year_of_purchase, data$month_of_purchase,sep = "")

data <- data %>% mutate(total_price = Quantity*UnitPrice)
View(data)

group_wise_purchase<- data %>% group_by(yearmonth) %>% summarise(revenue = sum(total_price))
View(group_wise_purchase)
x <- data.frame("SN" = 1:11, "yearmonth" = c(201101,201102,201103,201104,201105,201106,201107,201108,201109,201110,201111), 
                                             "revenue" = c(207634.8,169578.9,	210683.7,195675.3,	281587.2,278196.3,
                                                           187633.6,294118.8,279665.7,425948.3,	463738.4))
x

####base on monthly revenue
revenue_plot <- ggplot(x, aes(yearmonth,revenue, group = 1)) +
  geom_point() + 
  geom_line() + 
  labs(title = "summary of revenue", x = "Yearmonth", y = "monthly_revenue")
revenue_plot
###i need to know monthy wise inc or dec (percentange change)
library(quantmod)
x$pct_value<-Delt(x$revenue)
View(x)

 
growth_rate <- ggplot(x,aes(yearmonth,pct_value,group = 1))+geom_point()+
                                  geom_line()
growth_rate
top_list <- data %>% group_by(Country) %>% summarise(customer = n_distinct(CustomerID))
View(top_list)

###cluster on repetative customer
active<-data %>% group_by(yearmonth) %>% summarise(revenue = n_distinct(CustomerID))
barplot(active$revenue,names.arg = active$yearmonth)
## based on quantity
quantity<- data %>% group_by(yearmonth) %>% summarise(qual = sum(Quantity))
View((uk_quantity))
barplot(quantity$qual,names.arg = quantity$yearmonth)
###based on revenue
revenue<-data %>% group_by(yearmonth) %>% summarise(actual_revenue = mean(total_price))
View(revenue)
barplot(revenue$actual_revenue,names.arg = revenue$yearmonth)
### from this we found that they are some drop exactly on july
### it can be treated with many ways based on the industry
"### Here we are going to discuss about new customer and existing customer both
## both place major role in businee
### new customer ratio : It act a base like if you are losing exiting customers 
##  or attracting new customers
### retention : It tells about how many customer retain over a specfic period its
## important for any business"
### Let us analysis new customer ratio
purchase_date <- data %>% group_by(CustomerID) %>% summarise(purch_date = min(InvoiceDate))
names(purchase_date)
View(purchase_date)

purchase_date$year = as.numeric(format(purchase_date$purch_date, "%Y"))
purchase_date$month = as.numeric(format(purchase_date$purch_date, "%m"))
purchase_date$min_purchase = paste(purchase_date$year,purchase_date$month,sep = "")
###merging purchase data into actual data
data_full<-merge(data,purchase_date)
View(data_full)
data_full$usertype <- ifelse(data_full$yearmonth > data_full$min_purchase,"new","existing")
View(data_full)
###filtering to maintain the same group
data_cust<- filter(data_full,yearmonth != 201012 & yearmonth != 201112)
View(data_cust)
####based on total_price we segmentated new and existing customer for montlyinvoice
user_revenue <- data_cust %>% group_by(yearmonth,usertype) %>% summarise(rev_value=mean(total_price))
View(user_revenue)
### plotting the data
library(hrbrthemes)
library(viridis)
ggplot(data = user_revenue, aes(x = yearmonth, y = rev_value,group = usertype, color = usertype)) +geom_point()+ geom_line()+scale_color_viridis(discrete = TRUE, name="") +
                   theme(legend.position= ) +
                    ggtitle("Existing VS New") 
                   


'
Existing customers are showing a positive trend and tell us that 
our customer 
base is growing but new customers have a slight negative trend.'


##new customer ratio:

customer_new <- subset(data_cust, usertype == "new")
customer_new<- customer_new %>% group_by(yearmonth) %>% summarise(r1 = n_distinct(CustomerID))
View(customer_new)
customer_existing <- subset(data_cust, usertype == "existing")
customer_existing<- customer_existing %>% group_by(yearmonth) %>% summarise(r2 = n_distinct(CustomerID))
View(customer_existing)
###ratio :
ratio_new_customer <- cbind(customer_new[1],round(customer_new[-1] / customer_existing[-1],2))
View(ratio_new_customer)
###Visual presentation:

barplot(ratio_new_customer$r1,names.arg = ratio_new_customer$yearmonth)

"New Customer Ratio has declined as expected 
(we assumed on Feb, all customers were New)"

###Measuring of Retention Rate

"Retention rate help us to solve analyse the following question
1. Are the customers doing more or less repeat purchases over time? 
2. Rate at which we are losing customers getting better or worse?
3. What is the overall retention rate ? 
How and when are users returning back after their first purchase?"

"Here It uses Cohort Analysis"

"Cohort Analysis is a useful to analyze the long-term trends in 
customer retention and then calculate CLV (Customer Lifetime Value)."


library(lubridate)
cohort <- data_cust %>% 
  group_by(CustomerID) %>% 
  mutate(first_invoice = min(InvoiceDate)
         , f_invoice_month = floor_date(first_invoice,unit = 'month')
         ,cohort_index_day = InvoiceDate - first_invoice + 1 
         ,cohort_index_month = as.integer(cohort_index_day)/30
         ,cohort_index_month = cohort_index_month %>% round())%>% ungroup() %>% 
  group_by(f_invoice_month,cohort_index_month) %>% 
  summarise(unique = n_distinct(CustomerID))
cohort  
library(forcats)
library(stringr)
cohort %>% 
  ggplot(aes(y = f_invoice_month %>% 
               as.factor() %>% 
               fct_reorder(f_invoice_month,.desc = T),
             x = str_sub(cohort_index_month,1,7) %>% 
               fct_reorder(cohort_index_month))) +
  geom_tile(aes(fill = unique),color='white') + geom_text(aes(label = unique),size = 2) +
  scale_fill_gradient(low = 'yellow', high = 'red', space = 'Lab', na.value = 'white',title("cohort analysis on count"))

"Till now we analysed the metric of the eccommerence data...for better understanding"

#------------------------------------------------------------------------------------------#

"Now let start by segmenting customers using RFM Analysis (Recency,Frequency,Monetary)
and classify the customer based on low, med , high value based on clustering"


###RFM ANALYSIS
df_RFM <- data %>% 
  group_by(CustomerID,Country) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), montery= sum(total_price)/n_distinct(InvoiceNo))
  
df_RFM$Country <- as.character(df_RFM$Country)         
View(df_RFM)
df_rfm<-df_RFM %>% select(-starts_with("Country"))
View(df_rfm)


df_rfm$frequency <- as.numeric(df_rfm$frequency)
df <- subset(df_rfm, select = -c(CustomerID) )
str(df)
scaled_data = as.matrix(scale(df))
View(scaled_data)
str(df_rfm)
View(df_RFM)
library(factoextra)
library(NbClust)

nb=NbClust(scaled_data,distance="euclidean",min.nc = 2,max.nc = 15,method = "average")
hist(nos$Best.nc[1,], breaks = max(na.omit(nos$Best.nc[1,])))

nos$Best.nc

fviz_nbclust(scaled_data,kmeans,method = "wss")
fviz_nbclust(scaled_data,kmeans,method = "wss")+geom_vline(xintercept = 4,linetype = 2)

fviz_nbclust(scaled_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

set.seed(123)
fviz_nbclust(scaled_data, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

k4<-kmeans(scaled_data,4,iter.max=100,nstart=50,algorithm="Lloyd")
k4
k2<-kmeans(scaled_data,13,iter.max=100,nstart=50,algorithm="Lloyd")
k2
nb$All.index
indexvalue<-as.data.frame(nb$All.index)
plot(indexvalue$Silhouette,type="o")
silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, ])
}
k <- 2:13
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


library(purrr)
iss <- function(k) {
  kmeans(df_rfm,k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

k4<-kmeans(scaled_data,4,iter.max=100,nstart=50,algorithm="Lloyd")
k4

sil <- silhouette(k4$cluster, dist(scaled_data))
fviz_silhouette(sil)


fviz_cluster(k4, geom = "point", data = scaled_data) + ggtitle("k = 4")
table(k4$cluster)
aggregate(df_RFM, by=list(cluster=k4$cluster), mean)
data_kmeans<- cbind(df_RFM, cluster = k4$cluster)
View(data_kmeans)


colnms=c("recency","frequency","montery")
data_kmeans$overall <- rowSums(data_kmeans[,colnms])
View(data_kmeans)

group_mean <- data_kmeans %>% group_by(cluster) %>% summarise(mean_clust = mean(overall))
View(group_mean)

data_kmeans$segment[data_kmeans$cluster == 4]<-"high_value"
data_kmeans$segment[data_kmeans$cluster == 3]<-"mid_value"
data_kmeans$segment[data_kmeans$cluster == 2]<-"mid_value"
data_kmeans$segment[data_kmeans$cluster == 1]<-"low_value"

View(data_kmeans)
cluster_EDA <- select (data_kmeans,-c(cluster,overall))
View(cluster_EDA)

ggplot(data = cluster_EDA, aes(x = montery,y = Country,colour = segment)) +
  geom_point()+ggtitle("Segments", subtitle = "Using K-means Clustering")


data_filter <- subset(cluster_EDA, segment == "low_value")
data_filter


"Hence by using Hierarchical we can cluster df_rfm
inorder to show the visualization"

dist_mat <- dist(scaled_data, method = 'euclidean')
hclst <- hclust(dist_mat)
plot(hclst, cex = 0.6)
cut_avg <- cutree(hclst, k = 4)

df %>%
  mutate(cluster = cut_avg) %>%
  head

rect.hclust(hclst, k = 4, border = 2:5)
fviz_cluster(list(data = df, cluster = cut_avg))

"
The main strategies are clear:
High Value: Improve Retention
Mid Value: Improve Retention + Increase Frequency
Low Value: Increase Frequency

"


