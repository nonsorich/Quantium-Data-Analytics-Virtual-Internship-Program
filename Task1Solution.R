
#Load required libraries

library(tidyverse)
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(stringr)
library(lessR)
library(scales)


#Import transaction data set and purchase behavior data set
library(readxl)
QVI_transaction_data <- read_excel("~/Desktop/Quantium/QVI_transaction_data.xlsx")
View(QVI_transaction_data)


library(readr)
QVI_purchase_behaviour <- read_csv("C:/Users/abdul/Downloads/QVI_purchase_behaviour.csv")
View(QVI_purchase_behaviour)


#Examine closely the transaction data set
str(QVI_transaction_data)


#Convert DATE column to a date format
QVI_transaction_data$DATE <- as.Date(QVI_transaction_data$DATE, origin = "1899-12-30")

#Examine PROD_NAME column
head(QVI_transaction_data$PROD_NAME)

#Examine the words in PROD_NAME to see if there are any incorrect entries such as products that are not chips
productWords <- data.table(unlist(strsplit(unique(QVI_transaction_data$PROD_NAME), " ")))
setnames(productWords, 'words')

#Remove digits
productWords <- productWords[grepl("\\d", words) == FALSE, ]
View(productWords)

#Remove special characters.
productWords <- productWords[grepl("[:alpha:]", words), ]
View(productWords)

#Let's look at the words and the frequency of each word.
productWords %>% count(productWords$words) %>% arrange(n, desc())

#Remove salsa products.
QVI_transaction_data = QVI_transaction_data %>% filter(!str_detect(PROD_NAME, "Salsa"))

#Summarize the data to check for nulls and possible outliers.
summary(QVI_transaction_data)

#Filter the data set to find the outlier because the mean of the PROD_QTY is 1.908 and the max PROD_QTY is 200.
QVI_transaction_data %>% filter(PROD_QTY == 200)

#Filter out the outlier based on the PROD_QTY
QVI_transaction_data <- QVI_transaction_data[!(QVI_transaction_data$PROD_QTY==200),]
QVI_transaction_data %>% filter(PROD_QTY == 200)

#Re-examine transaction data
summary(QVI_transaction_data)

#Check for missing DATE in the DATE column
FullSeq <- seq.Date(from = min(QVI_transaction_data$DATE), to = max(QVI_transaction_data$DATE), by = 1)
Missing <- FullSeq[!FullSeq %in% QVI_transaction_data$DATE]


#Calculate the total sales for each day
Total_Sales_D = QVI_transaction_data %>% group_by(DATE) %>% summarise(TOTAL_SALES = sum(TOT_SALES)) %>%
  arrange(DATE, desc())
View(Total_Sales_D) 

#Check the total number of days transactions occurred in our dataset.
length(Total_Sales_D$DATE)


#Set plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#Plot transactions from July 218 - June 2019
ggplot(Total_Sales_D, aes(x = DATE, y = TOTAL_SALES)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions from July 2018 - June 2019") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Filter out the transactions that occurred in the month of December and look at individual days
Total_Sales_Dec = Total_Sales_D %>% filter(DATE > "2018-11-30" & DATE < "2019-01-01")
View(Total_Sales_Dec)

#Plot the daily transactions in the month of December.
ggplot(Total_Sales_Dec, aes(x = DATE, y = TOTAL_SALES)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions in the month of Dec") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Creating another Pack size column from the Product name column
QVI_transaction_data$PACK_SIZE <- str_extract(QVI_transaction_data$PROD_NAME,  "[-+.e0-9]*\\d")


#Convert PACK_SIZE to numeric and sorting in ascending
QVI_transaction_data$PACK_SIZE <- as.numeric(as.character(QVI_transaction_data$PACK_SIZE))
class(QVI_transaction_data$PACK_SIZE)
Transaction_data = QVI_transaction_data %>% arrange(PACK_SIZE)
summary(Transaction_data$PACK_SIZE)

#Plot a histogram showing the distribution of the PACk SIZE
hist(Transaction_data$PACK_SIZE)

#Creating brand using the first word from the product name column
Transaction_data$BRAND <- str_extract(Transaction_data$PROD_NAME,"(\\w+)") 
View(Transaction_data)

#Get the frequency of each brand name
Brand_Name = Transaction_data %>% group_by(BRAND) %>% count(BRAND)

#Sort the frequency of the brand name in descending.
Brand_Name = Brand_Name %>% arrange(n, desc(n))
View(Brand_Name)

#Clean Brand name to make it consistent
Transaction_data$BRAND[Transaction_data$BRAND == "Red"]<-"RRD"
Transaction_data$BRAND[Transaction_data$BRAND == "Snbts"]<-"Sunbites"
Transaction_data$BRAND[Transaction_data$BRAND == "Infzns"]<-"Infuzions"
Transaction_data$BRAND[Transaction_data$BRAND == "Ww"]<-"Woolworths"
Transaction_data$BRAND[Transaction_data$BRAND == "Smith"]<-"Smiths"
Transaction_data$BRAND[Transaction_data$BRAND == "NATURAL"]<-"Natural"
Transaction_data$BRAND[Transaction_data$BRAND == "Dorito"]<-"Doritos"
Transaction_data$BRAND[Transaction_data$BRAND == "Grain"]<-"Grnwves"

#Get the frequency of the brand name after regularizing it.
Brand_Name_C = Transaction_data %>% group_by(BRAND) %>% count(BRAND)

#Sort the brand name in descending order after making regularizing it
Brand_Name_C = Brand_Name_C %>% arrange(desc(n))
View(Brand_Name_C)

#Examine the customer dataset
str(QVI_purchase_behaviour)

summary(QVI_purchase_behaviour)


#Group the Lifestage column by the frequency of occurrence and plot a bar chart
QVI_purchase_behaviour %>% group_by(LIFESTAGE) %>% count(LIFESTAGE) %>% arrange(desc(n)) 
BarChart(LIFESTAGE, data=QVI_purchase_behaviour, horiz=TRUE)

#Group the premium customer by the frequency of occurrence and plot a bar chart
QVI_purchase_behaviour %>% group_by(PREMIUM_CUSTOMER) %>% count(PREMIUM_CUSTOMER) %>% arrange(desc(n)) 
BarChart(PREMIUM_CUSTOMER, data=QVI_purchase_behaviour, horiz=TRUE)

#Merge transaction data to purchase behavior data
Data_M <- merge(Transaction_data, QVI_purchase_behaviour, by = "LYLTY_CARD_NBR")
View(Data_M)

#Examine the merged dataset
str(Data_M)

summary(Data_M)


#Total sales by LIFESTAGE and PREMIUM_CUSTOMER
Total_Sales = Data_M %>% group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% summarise(TOTAL_SALES = sum(TOT_SALES))
View(Total_Sales)


#Convert the premium customer and lifestage columns from character to a factor
Total_Sales$LIFESTAGE  <- as.factor(Total_Sales$LIFESTAGE)
Total_Sales$PREMIUM_CUSTOMER  <- as.factor(Total_Sales$PREMIUM_CUSTOMER)
is.factor(Total_Sales$PREMIUM_CUSTOMER)


#Visualize total sale with life stages and premium customer
Total_Sales %>% ggplot(aes(x = LIFESTAGE, y = TOTAL_SALES, fill = PREMIUM_CUSTOMER)) +
  geom_col() + 
  scale_y_continuous(labels = comma) + 
  labs(x = "Lifestage",
       y = "Premium customer flag", 
       title = "Proportion of sales") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

#Group premium customer and lifestage by the frequency of premium customer
No_of_Premium_c = Data_M %>%  group_by(PREMIUM_CUSTOMER, LIFESTAGE) %>% count(PREMIUM_CUSTOMER) 
View(No_of_Premium_c)

#Plot a bar chart showing the premium customers and the number of customers
No_of_Premium_c %>% ggplot(aes(x = LIFESTAGE, y = n, fill = PREMIUM_CUSTOMER)) +
  geom_col() + 
  scale_y_continuous(labels = comma) + 
  labs(x = "Lifestage",
       y = "Premium customer flag", 
       title = "Proportion of Customers") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

#Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER  
Avg_Units <- Data_M %>% group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% summarise(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)) %>% arrange(desc(AVG)) 
View(Avg_Units) 

#Plot a bar chart for Avg units per customer by lifestage and premium customer
ggplot(data = Avg_Units, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#Exame average price by per unit by LIFESTAGE and PREMIUM_CUSTOMER
AVG_PRICE <- Data_M %>% group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% summarise(Avg = sum(TOT_SALES)/sum(PROD_QTY)) %>% arrange(desc(Avg))
View(AVG_PRICE)

#Plot a chart showing the average price by Lifestage and premium customer
ggplot(data = AVG_PRICE, aes(weight = Avg, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#Findings

#2018-12-25 is missing in the DATE column and it's assumed that the store was closed on Christmas day.
#Sales increased in the days before Christmas day.</li>
#Sales are mainly coming from older families (budget), retirees (mainstream) and young singles/couples (mainstream).
#Mainstream retirees and mainstream young singles/couples contribute more to the sales and it isn't the case with older families budget.
#Each of the premium segment in older families and young families buy more chips.</li>
#Mainstream mid-age single/couples and mainstream young singles/couples spend more per chips compared to other premium customers in their category.
    
  
