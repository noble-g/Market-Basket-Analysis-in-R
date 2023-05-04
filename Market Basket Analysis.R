library("arules")
library("arulesViz")
library("readxl")
library("tidyverse")
library("plyr")
library("dplyr")
library("ggplot2")

#reading in the external data - online retail dataset
df<- read_excel(file.choose())

#work on a fraction of the data to save time and computer power
# filter data where country is equal to France
dff<- df%>%
    filter(Country  == 'France')
head(dff)


# EDA
view(dff)
head(dff)
dim(dff)
nrow(dff)
ncol(dff)
summary(dff)

#Reading in the internal data - Groceries dataset
data("Groceries")
head(Groceries)
summary(Groceries)
dim(Groceries)
nrow(Groceries)
ncol(Groceries)

rulexx <- apriori(Groceries)
rulexx

#cleaning and transforming the dataset

# on a second tought, I wouldn't be needing the col with the missing values, so ...
#df<- df[complete.cases(df),]
#dim(df)

# Reshaping dataset
# grouping all items on the same InvoiceNo and invoice date to be form a transaction

# trying to understand the intricacies of how function works with ddply
func = function(x){
  paste(x$Description, collapse = ",")
}

func(dff)

transaction_dff<- ddply(dff, c("InvoiceNo", "InvoiceDate"), function(x)paste(x$Description, collapse = ","))

?ddply()
view(transaction_dff)
head(transaction_dff)
dim(transaction_dff)

transaction_dff<- ddply(dff, c("InvoiceNo", "InvoiceDate"), func())

aggregate(dff, by = list("InvoiceNo", "InvoiceDate"), FUN = func(x))
#?aggregate

dff["Description"]

transaction_df1<- dff %>% group_by(InvoiceNo, InvoiceDate) %>% do(func(dff["Description"]))

transaction_dff<- dff %>% 
                  group_by(InvoiceNo, InvoiceDate) %>% 
                  dplyr::summarize(Description = paste(df$Description, collapse = ",")) %>%
                  as.data.frame()


head(transaction_dff)
view(transaction_dff)
view(transaction_dff)

# loop through the items to make each item into a column
#while TRUE:

# rename V1 to transactions
colnames(transaction_dff)<-c("transaction")

# transform transaction_dff to the one hot encoded format

# first we try to know the unique individual items
items <- unique(dff$Description)
length(items)

# InvoiceNo and InvoiceDate are no more useful to us
transaction_dff$InvoiceNo <- NULL
transaction_dff$InvoiceDate <- NULL

write.csv(transaction_dff, "retail transactions.csv", quote = FALSE, row.names = FALSE)

transactions<- read.transactions('./retail transactions.csv',format = 'basket', sep= ',')
view(transactions)
summary(transactions)
inspect(transactions)

rules<- apriori(transactions, parameter = list(
  minlen = 2, 
  maxlen = 3,
  supp = .1,
  conf = .8
))

?apriori

rules
view(inspect(rules))
plot(rules)

# Save rules as PMML
write.PMML(rules, file = "rules.xml")
# Read rules
read.PMML("rules.xml")
