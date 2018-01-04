install.packages("data.table")
library(data.table)

Customer <- fread("rawdata/01_Customer.txt")
Shopping <- fread("rawdata/02_Shopping.txt")
Exshopping <- fread("rawdata/03_Exshopping.txt")
Shopitem <- fread("rawdata/04_Shopitem.txt", encoding = "UTF-8")

Customer
