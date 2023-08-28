#read the data
Retail<- read.csv("clipboard",sep='\t', header = T)
str(Retail)

#convert InvoiceDate into date format from character vector
str(Retail$InvoiceDate)
max(Retail$InvoiceDate)
Retail$InvoiceDate<- as.Date(Retail$InvoiceDate, format = "%Y-%m-%d")
#run libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

# data cleaning
# remove blank rows in the data frame
Retail %>% 
  na.omit(Retail)

#there are many rows with quantity less than 1. We want to work with data where quantity is more than 0.
Retail<- Retail %>% 
  filter(Quantity>0)
#around 4420 rows have been removed
# check for duplicated rows
Retail %>% 
  distinct() %>% 
  View()

#create an analysis date so as to find out the recency
analysis_date = max(Retail$InvoiceDate)

# we will need to create a column called amount as we will need it for the rfm analysis
Retail<-Retail %>% 
  mutate(Amount = Quantity*UnitPrice) 
#rfm analysis
library(rfm)
?rfm_table_order
rfm_results<- rfm_table_order(data = Retail,customer_id = CustomerID, order_date = InvoiceDate,
                              revenue = Amount, analysis_date = analysis_date)
#creating new segments
segment_titles <- c("Champions","Loyal Customers","Potential Loyalist","New Customers","Promising",
                    "Need Attention","About To Sleep","At Risk","Can't Lose Them","Hibernating",
                    "Lost")
#create minimum and maximum values for Regency, Frequency and Monetary for the above segments
r_low<- c(4,2,3,4,3,2,2,1,1,1,1) # minimum value of recency
r_high<- c(5,5,5,5,4,3,3,2,1,2,2)#maximum value of recency
f_low<- c(4,3,1,1,1,2,2,2,4,1,1)# minimum value of frequency
f_high<- c(5,5,3,1,1,3,2,5,5,2,2)# maximum value of frequency
m_low<- c(4,3,1,1,1,2,2,2,4,1,1)# minimum value of monetary
m_high<- c(5,5,3,1,1,3,2,5,5,2,2)# maximum value of monetary

#create new data frame where a new column called segments which will have the
#segment titles for each customer ID based upon the RFM scores 
divisions<- rfm_segment(rfm_results,segment_titles,r_low,r_high,f_low,f_high,m_low,m_high) 
head(divisions,5)

#convert the frequency of column name segment into count and percentage
segments<- divisions %>% 
  count(segment) %>% 
  arrange(desc(n)) %>% 
  rename(SEGMENT = segment, FREQUENCY = n) %>% 
  mutate(PERCENTAGE = (FREQUENCY/sum(FREQUENCY)*100))

# make plots
divisions %>% 
 rfm_plot_median_recency()

divisions %>% 
rfm_plot_median_frequency()

divisions %>%
rfm_plot_median_monetary()



