# install.packages("data.table")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("reshape2")


library(data.table)
library(plyr)
library(ggplot2)
library(reshape2)
library(proxy)

#######################################################
############### Environment setting ###################
#######################################################

### load and save rawdata

# Customer <- fread("rawdata/01_Customer.txt")
# Shopping <- fread("rawdata/02_Shopping.txt")
# ExShopping <- fread("rawdata/03_ExShopping.txt")
# Product <- fread("rawdata/04_Product.txt", encoding = "UTF-8")
# 
# head(Customer)
# head(Shopping)
# head(ExShopping)
# head(Product)
# 
# str(Customer)
# str(Shopping)
# str(ExShopping)
# str(Product)
# 
# save(list = ls(), file = "Rdata/Loaded data with DT.RData")

load("Rdata/Loaded data with DT.RData")
load("Rdata/Product by Shopping BIZ_UNIT.RData")
load("Rdata/Shopping Transaction by BIZ_UNIT.RData")


#############################################
############### EDA - Customer ###############
#############################################

# # 성별, 나이대별 군집변수 만들기
# Customer <- as.data.frame(Customer)
# Customer["class"] <- paste(Customer$GENDER, Customer$AGE_PRD, sep = "_")
# Customer <- as.data.table(Customer)


# Grasping number of people by GENDER
Customer[, .N, by = GENDER]
ggplot(Customer, aes(x=factor(GENDER))) + geom_bar()


# Grasping number of people by AGE
Customer[, .N, by = AGE_PRD]
ggplot(Customer, aes(x=factor(AGE_PRD))) + geom_bar()


# Grasping number of people by HOM
Customer[, .N, by = HOM_PST_NO]
Customer[, .N, by = HOM_PST_NO][order(-N),] # 내림차순
ggplot(Customer, aes(x=factor(HOM_PST_NO))) + geom_bar()


# Grasping number of people by GENDER & AGE
with(Customer, table(GENDER, AGE_PRD))
count(Customer, c("GENDER","AGE_PRD")) # same output

Count_GENDER_AGE <- as.data.frame(with(Customer, table(GENDER, AGE_PRD)))
Count_GENDER_AGE

ggplot(Count_GENDER_AGE, aes(x=AGE_PRD, y = Freq, fill=GENDER)) + geom_bar(stat="identity")


# Grasping number of people by GENDER & HOM
with(Customer, table(GENDER, HOM_PST_NO))
count(Customer, c("GENDER","HOM_PST_NO"))

Count_GENDER_HOM <- as.data.frame(with(Customer, table(GENDER, HOM_PST_NO)))
Count_GENDER_HOM

ggplot(Count_GENDER_HOM, aes(x=HOM_PST_NO, y = Freq, fill=GENDER)) + geom_bar(stat="identity")


# Grasping number of people by AGE & HOM
with(Customer, table(AGE_PRD, HOM_PST_NO))
count(Customer, c("AGE_PRD","HOM_PST_NO"))

Count_AGE_HOM <- as.data.frame(with(Customer, table(AGE_PRD, HOM_PST_NO)))
Count_AGE_HOM

ggplot(Count_AGE_HOM, aes(x=HOM_PST_NO, y = Freq, fill=AGE_PRD)) + geom_bar(stat="identity")


# Grasping number of people by GENDER, AGE, HOM
with(Customer, table(GENDER, AGE_PRD, HOM_PST_NO))
head(count(Customer, c("GENDER","AGE_PRD","HOM_PST_NO")))

Count_GENDER_AGE_HOM <- count(Customer, c("GENDER","AGE_PRD","HOM_PST_NO"))
Count_GENDER_AGE_HOM <- as.data.frame(with(Customer, table(GENDER, AGE_PRD, HOM_PST_NO)))
head(Count_GENDER_AGE_HOM)

# ggplot(Count_GENDER_AGE_HOM, aes(x=HOM_PST_NO, y = Freq, fill=AGE_PRD)) + geom_bar(stat="identity")
# how to plot multiple criteria frequency





#############################################
############### EDA - Product ###############
#############################################

head(Product)
setkey(Product, PD_S_C)

Product[PD_S_C == 100,] # 상품코드는 같지만 업종이 다름 (여러개의 상품)


# Classification product by BIZ_UNIT

# Product_A01 <- Product[BIZ_UNIT == 'A01',]
# Product_A02 <- Product[BIZ_UNIT == 'A02',]
# Product_A03 <- Product[BIZ_UNIT == 'A03',]
# Product_A04 <- Product[BIZ_UNIT == 'A04',]
# Product_A05 <- Product[BIZ_UNIT == 'A05',]
# 
# save(list = c("Product_A01","Product_A02","Product_A03","Product_A04","Product_A05"),
#      file = "Rdata/Product by Shopping BIZ_UNIT.RData")


# Number of Product by Shopping BIZ_UNIT & PD_M_NM (descending order) - 중분류에 몇개 상품?

Product_A01[, .N, by = PD_M_NM][order(-N),] # 중분류명 51개
Product_A02[, .N, by = PD_M_NM][order(-N),] # 중분류명 411개
Product_A03[, .N, by = PD_M_NM][order(-N),] # 중분류명 99개
Product_A04[, .N, by = PD_M_NM][order(-N),] # 중분류명 163개
Product_A05[, .N, by = PD_M_NM][order(-N),] # 중분류명 36개

ggplot(Product_A01[, .N, by = PD_M_NM][order(-N),], 
       aes(x=factor(PD_M_NM, levels = PD_M_NM[order(Product_A01[, .N, by = PD_M_NM][order(-N),]$N)]), y=N)) + geom_bar(stat="identity")

ggplot(Product_A02[, .N, by = PD_M_NM][order(-N),], 
       aes(x=factor(PD_M_NM, levels = PD_M_NM[order(Product_A02[, .N, by = PD_M_NM][order(-N),]$N)]), y=N)) + geom_bar(stat="identity")

ggplot(Product_A03[, .N, by = PD_M_NM][order(-N),], 
       aes(x=factor(PD_M_NM, levels = PD_M_NM[order(Product_A03[, .N, by = PD_M_NM][order(-N),]$N)]), y=N)) + geom_bar(stat="identity")

ggplot(Product_A04[, .N, by = PD_M_NM][order(-N),], 
       aes(x=factor(PD_M_NM, levels = PD_M_NM[order(Product_A04[, .N, by = PD_M_NM][order(-N),]$N)]), y=N)) + geom_bar(stat="identity")

ggplot(Product_A05[, .N, by = PD_M_NM][order(-N),], 
       aes(x=factor(PD_M_NM, levels = PD_M_NM[order(Product_A05[, .N, by = PD_M_NM][order(-N),]$N)]), y=N)) + geom_bar(stat="identity")


Product_A01_TOP20 <- head(Product_A01[, .N, by = PD_M_NM][order(-N),], 20)
Product_A02_TOP20 <- head(Product_A02[, .N, by = PD_M_NM][order(-N),], 20)
Product_A03_TOP20 <- head(Product_A03[, .N, by = PD_M_NM][order(-N),], 20)
Product_A04_TOP20 <- head(Product_A04[, .N, by = PD_M_NM][order(-N),], 20)
Product_A05_TOP20 <- head(Product_A05[, .N, by = PD_M_NM][order(-N),], 20)

ggplot(Product_A01_TOP20, 
       aes(x=factor(PD_M_NM, levels = PD_M_NM[order(Product_A01_TOP20$N)]), y=N)) + geom_bar(stat="identity")

ggplot(Product_A02_TOP20, 
       aes(x=factor(PD_M_NM, levels = PD_M_NM[order(Product_A02_TOP20$N)]), y=N)) + geom_bar(stat="identity")

ggplot(Product_A03_TOP20, 
       aes(x=factor(PD_M_NM, levels = PD_M_NM[order(Product_A03_TOP20$N)]), y=N)) + geom_bar(stat="identity")

ggplot(Product_A04_TOP20, 
       aes(x=factor(PD_M_NM, levels = PD_M_NM[order(Product_A04_TOP20$N)]), y=N)) + geom_bar(stat="identity")

ggplot(Product_A05_TOP20, 
       aes(x=factor(PD_M_NM, levels = PD_M_NM[order(Product_A05_TOP20$N)]), y=N)) + geom_bar(stat="identity")

# Category of Product by BIZ_UNIT
Product_A01[, sort(unique(PD_H_NM))]
Product_A02[, sort(unique(PD_H_NM))]
Product_A03[, sort(unique(PD_H_NM))]
Product_A04[, sort(unique(PD_H_NM))]
Product_A05[, sort(unique(PD_H_NM))]

# Division of Product by BIZ_UNIT
Product_A01[, sort(unique(PD_M_NM))]
Product_A02[, sort(unique(PD_M_NM))]
Product_A03[, sort(unique(PD_M_NM))]
Product_A04[, sort(unique(PD_M_NM))]
Product_A05[, sort(unique(PD_M_NM))]

# Section of Product by BIZ_UNIT # 소분류명 겹치는게 있ㅇ
Product_A01[, sort(unique(PD_S_NM))]
Product_A02[, sort(unique(PD_S_NM))]
Product_A03[, sort(unique(PD_S_NM))]
Product_A04[, sort(unique(PD_S_NM))]
Product_A05[, sort(unique(PD_S_NM))]


### Same PD_S_NM but different PD_S_C
### 소분류명도 같고 중분류명도 같은데 대분류명은 다른 아이템이 있음

# A01 : 19 items
nrow(Product_A01)
length(Product_A01[, sort(unique(PD_S_NM))])
Product_A01[, .N, by = PD_S_NM][N!=1,] # 소분류명은 같은데 코드번호가 다름
# Product_A01[Product_A01$PD_S_NM == "디자이너"]
# Product_A01[Product_A01$PD_S_NM == "어덜트"]
Product_A01_same_S_NM <- Product_A01[PD_S_NM %in% Product_A01[, .N, by = PD_S_NM][N!=1,]$PD_S_NM,]
Product_A01_same_S_NM[order(PD_S_NM)]

# A02 : 55 items
nrow(Product_A02)
length(Product_A02[, sort(unique(PD_S_NM))])
Product_A02[, .N, by = PD_S_NM][N!=1,] 
Product_A02_same_S_NM <- Product_A02[PD_S_NM %in% Product_A02[, .N, by = PD_S_NM][N!=1,]$PD_S_NM,]
Product_A02_same_S_NM[order(PD_S_NM)]

# A03 : Nothing
# nrow(Product_A03)
# length(Product_A03[, sort(unique(PD_S_NM))])
# Product_A03[, .N, by = PD_S_NM][N!=1,] 
# Product_A03_same_S_NM <- Product_A03[PD_S_NM %in% Product_A03[, .N, by = PD_S_NM][N!=1,]$PD_S_NM,]
# Product_A03_same_S_NM[order(PD_S_NM)]

# A04 : 25 items
nrow(Product_A04)
length(Product_A04[, sort(unique(PD_S_NM))])
Product_A04[, .N, by = PD_S_NM][N!=1,] 
Product_A04_same_S_NM <- Product_A04[PD_S_NM %in% Product_A04[, .N, by = PD_S_NM][N!=1,]$PD_S_NM,]
Product_A04_same_S_NM[order(PD_S_NM)]

# A05 : Nothing
# nrow(Product_A05)
# length(Product_A05[, sort(unique(PD_S_NM))])
# Product_A05[, .N, by = PD_S_NM][N!=1,] 
# Product_A05_same_S_NM <- Product_A05[PD_S_NM %in% Product_A05[, .N, by = PD_S_NM][N!=1,]$PD_S_NM,]
# Product_A05_same_S_NM[order(PD_S_NM)]


# # 제휴사별 대분류 품목 갯수
# Product[, .N, by = .(BIZ_UNIT, PD_H_NM)]
# which.max(Product[, .N, by = .(BIZ_UNIT, PD_H_NM)]$N) # 131 번째
# sort(Product[, .N, by = .(BIZ_UNIT, PD_H_NM)]$N) # 218개
# Product[, .N, by = .(BIZ_UNIT, PD_H_NM)][131,]
# 
# # 제휴사와 대분류별 중분류 품목 갯수
# Product[, .N, by = .(BIZ_UNIT, PD_H_NM, PD_M_NM)]
# which.max(Product[, .N, by = .(BIZ_UNIT, PD_H_NM, PD_M_NM)]$N) # 250번째
# sort(Product[, .N, by = .(BIZ_UNIT, PD_H_NM, PD_M_NM)]$N) # 76개
# Product[, .N, by = .(BIZ_UNIT, PD_H_NM, PD_M_NM)][250,]


# Category of Product by BIZ_UNIT
(Product[BIZ_UNIT == "A01", sort(unique(PD_H_NM))])
(Product[BIZ_UNIT == "A02", sort(unique(PD_H_NM))])
(Product[BIZ_UNIT == "A03", sort(unique(PD_H_NM))])
(Product[BIZ_UNIT == "A04", sort(unique(PD_H_NM))])
(Product[BIZ_UNIT == "A05", sort(unique(PD_H_NM))])

# 제휴사별 중분류 품목
(Product[BIZ_UNIT == "A01", sort(unique(PD_M_NM))])
(Product[BIZ_UNIT == "A02", sort(unique(PD_M_NM))])
(Product[BIZ_UNIT == "A03", sort(unique(PD_M_NM))])
(Product[BIZ_UNIT == "A04", sort(unique(PD_M_NM))])
(Product[BIZ_UNIT == "A05", sort(unique(PD_M_NM))])



##########################################################
############### EDA - Shopping Transaction ###############
##########################################################

Shopping

# Shopping_A01 <- Shopping[BIZ_UNIT == "A01"]
# Shopping_A02 <- Shopping[BIZ_UNIT == "A02"]
# Shopping_A03 <- Shopping[BIZ_UNIT == "A03"]
# Shopping_A04 <- Shopping[BIZ_UNIT == "A04"]
# Shopping_A05 <- Shopping[BIZ_UNIT == "A05"]

# save(list = c("Shopping_A01","Shopping_A02","Shopping_A03","Shopping_A04","Shopping_A05"),
#      file = "Rdata/Shopping Transaction by BIZ_UNIT.RData")




# 영수증 번호 별 구매 상품 갯수
Shopping_A01[, .N, by = RCT_NO][order(-N)]
Shopping_A02[, .N, by = RCT_NO][order(-N)]
Shopping_A03[, .N, by = RCT_NO][order(-N)]
Shopping_A04[, .N, by = RCT_NO][order(-N)]
Shopping_A05[, .N, by = RCT_NO][order(-N)]


# 그런데 영수증 번호가 같은데 구매날짜가 다른 경우 존재
# 위와 같은 경우에는 서로 다른 결제 건으로 구분하여 분석 (by Lpoint)
# 아래의 경우와 같이 영수증 번호 = 380096 / 구매일자 = 20150111, 20150802
# 또, 소분류 코드가 같은데도 중복으로 나타나고 구매금액이 다른 이유는????
# BIZ_UNIT = "A01", PD_S_C = 19 -> 소분류명 : 채소 -> 채소의 여러종류 구매
Shopping_A01[RCT_NO == 380096] 
Shopping_A01[RCT_NO == 380096][order(DE_DT, PD_S_C)]

head(Product)
str(Product)
Product[BIZ_UNIT == "A01" & PD_S_C == 19,]

# ID가 15652 인 고객은 A01에서 92번 거래하였음
Shopping_A01[ID == 15652,][, .N, by = RCT_NO] 



# 제휴사 및 고객별 총 거래횟수, 총 거래액 (총액단위:만원)
Shopping_A01_NUM_AMT <- Shopping_A01[, list(BUY_NUM = length(unique(RCT_NO)), 
                                            AMOUNT = sum(BUY_AM)/10000), 
                                     by = ID]

Shopping_A02[, list(BUY_NUM = length(unique(RCT_NO)), AMOUNT = sum(BUY_AM)/10000), by = ID]
Shopping_A03[, list(BUY_NUM = length(unique(RCT_NO)), AMOUNT = sum(BUY_AM)/10000), by = ID]
Shopping_A04[, list(BUY_NUM = length(unique(RCT_NO)), AMOUNT = sum(BUY_AM)/10000), by = ID]
Shopping_A05[, list(BUY_NUM = length(unique(RCT_NO)), AMOUNT = sum(BUY_AM)/10000), by = ID]


# A01 제휴사 1억원 이상 구매고객 그래프
ggplot(data = Shopping_A01_NUM_AMT[AMOUNT>=10000], 
       aes(x=factor(ID, levels = Shopping_A01_NUM_AMT[AMOUNT>=10000][order(-AMOUNT)]$ID),y=AMOUNT)) +
  geom_bar(stat = "identity")


Shopping_A01_NUM_AMT[order(-AMOUNT)]
Shopping_A01_NUM_AMT[order(-BUY_NUM)]

# setkey(Shopping_A01, RCT_NO)
a <- Shopping_A01[, list(ID = unique(ID), v1 = sum(BUY_AM)), by = RCT_NO]
a[ID==5]
sum(a[ID==13087]$v1)
max(a[ID==5]$v1)
min(a[ID==13087]$v1)

head(tapply(a$v1, a$ID, max))


Shopping_A01[RCT_NO==2108]

setkey(Shopping_A01, ID)
setkey(a, ID)
Shopping_A01[, list(BUY_NUM = length(unique(RCT_NO)), 
                    AMOUNT = sum(BUY_AM)/10000), 
             by = ID]


Shopping_A01[, sum(BUY_AM), keyby = ID, RCT_NO]


Shopping_A01_NUM_AMT[, list(MEAN = AMOUNT/BUY_NUM), by = ID][order(-MEAN)]


Shopping_A01[, .N, by = PD_S_C][order(-N)]


a[which.max(a$AMOUNT),] # 13087번 고객이 1년 동안 구매금액 TOP 1 - 6억
Shopping_A01[ID==13087][,sum(BUY_AM)]
Shopping_A01[ID==4008][,sort(unique(RCT_NO))]

Shopping_A01[RCT_NO == 1]

head(Shopping_A01[, c(RCT_NO = RCT_NO, AMOUNT = sum(BUY_AM)), by = RCT_NO],20)

tables()


for (i in 1:10) {
  Shopping_A01
}



##########################################################
############# EDA - ExShopping Transaction ###############
##########################################################

head(ExShopping)
head(Shopping_A01_NUM_AMT[order(-BUY_NUM)])
Shopping[ID == 617, .N, by = RCT_NO]
ExShopping[ID == 617]



#####################################################
############# Customer Classification ###############
#####################################################

Customer <- Customer[order(ID)]
Customer$AGE_PRD <- as.numeric(gsub("PRD","",Customer$AGE_PRD))

for (i in 1:length(Customer$ID)){
  if (is.na(Customer[i, "HOM_PST_NO"])) {
    Customer[i, "HOM_PST_NO"] <- 999
  }
}

Customer <- as.data.frame(Customer)

str(Customer)

library(proxy)

a <- Customer[1:1000,]
head(a)

d <- dist(as.matrix(a[-1]), method = "cosine")
fit <- hclust(d, method = "ave")
par(mfrow=c(1,2))
fit
plot(fit)
plot(fit, hang = -1)



Customer$ID <- factor(Customer$ID)
Customer$GENDER <- factor(Customer$GENDER)
Customer$AGE_PRD <- as.numeric(gsub("PRD","",Customer$AGE_PRD))
Customer$HOM_PST_NO <- factor(Customer$HOM_PST_NO)

with(Customer, table(GENDER, AGE_PRD))

Customer <- as.data.frame(Customer[order(ID)])
head(Customer)
str(Customer)

################################################################
############# User-based collaborative filtering ###############
################################################################



head(Product_A01)
head(Shopping_A01)

Product_A01_PD_M_NM <- data.table("PD_S_C" = Product_A01$PD_S_C, 
                                  "PD_M_NM" = Product_A01$PD_M_NM)

Shopping_A01_PD_M_NM <- data.table("ID" = Shopping_A01$ID,
                                   "RCT_NO" = Shopping_A01$RCT_NO,
                                   "PD_S_C" = Shopping_A01$PD_S_C)

setkey(Shopping_A01_PD_M_NM, PD_S_C)
setkey(Product_A01_PD_M_NM, PD_S_C)

Shopping_A01_PD_M_NM <- Shopping_A01_PD_M_NM[Product_A01_PD_M_NM,]

Shopping_A01_PD_M_NM_by_ID <- Shopping_A01_PD_M_NM[, list("PD_M_NM" = list(sort(PD_M_NM))), by = ID]
# Shopping_A01_PD_M_NM_by_ID[order(ID)]
# Shopping_A01_PD_M_NM_by_ID[ID == 1]$PD_M_NM
# Shopping_A01_PD_M_NM_by_ID[ID == 2]$PD_M_NM




Shopping_A01[ID == 10357]
a <- Shopping_A01_PD_M_NM[, list("PD_M_NM" = unlist(sort(PD_M_NM))), by = ID]
a$count <- 1

# ID / PD_M_NM / COUNTS 
a[, sum(count), by = .(ID, PD_M_NM)][ID == 10357]
a1 <- a[, sum(count), by = .(ID, PD_M_NM)]
a1



User_based_A01 <- matrix(nrow = length(Customer$ID),
                         ncol = length(unique(Product_A01$PD_M_NM)))
colnames(User_based_A01) <- Product_A01[, sort(unique(PD_M_NM))]
rownames(User_based_A01) <- Customer[,sort(unique(.SD$ID))]

colnames(User_based_A01)[1]


View(xtabs(V1 ~ ID + PD_M_NM, data = a1))









# for (i in 1:length(Customer$ID)){
#   for (j in 1:length(unique(Product_A01$PD_M_NM))){
#     if (is.null(unlist(Shopping_A01_PD_M_NM_by_ID[ID == i]$PD_M_NM))) {
#       User_based_A01[i,j] <- 0
#     } else {
#       User_based_A01[i,j] <- sum(colnames(User_based_A01)[j] == unlist(Shopping_A01_PD_M_NM_by_ID[ID == i]$PD_M_NM))
#     } 
#   }
# }
# View(User_based_A01)

system.time(
  for (i in 1:10){
    for (j in 1:length(unique(Product_A01$PD_M_NM))){
      if (is.null(unlist(Shopping_A01_PD_M_NM_by_ID[ID == i]$PD_M_NM))) {
        User_based_A01[i,j] <- 0
      } else {
        User_based_A01[i,j] <- sum(colnames(User_based_A01)[j] == unlist(Shopping_A01_PD_M_NM_by_ID[ID == i]$PD_M_NM))
      }
    }
  }
)
View(User_based_A01)

system.time(
  for (i in 1:50){
    for (j in 1:length(unique(Product_A01$PD_M_NM))){
      if (is.null(unlist(Shopping_A01_PD_M_NM_by_ID[ID == i]$PD_M_NM))) {
        User_based_A01[i,j] <- 0
      } else {
        User_based_A01[i,j] <- sum(colnames(User_based_A01)[j] == unlist(Shopping_A01_PD_M_NM_by_ID[ID == i]$PD_M_NM))
      }
    }
  }
)
View(User_based_A01)

system.time(
  for (i in 1:100){
    for (j in 1:length(unique(Product_A01$PD_M_NM))){
      if (is.null(unlist(Shopping_A01_PD_M_NM_by_ID[ID == i]$PD_M_NM))) {
        User_based_A01[i,j] <- 0
      } else {
        User_based_A01[i,j] <- sum(colnames(User_based_A01)[j] == unlist(Shopping_A01_PD_M_NM_by_ID[ID == i]$PD_M_NM))
      } 
    }
  }
)
View(User_based_A01)

(df <- data.frame(x = c(10,50,100), y = c(2.95, 18.25, 30)))
ggplot(df, aes(x = df$x, y = df$y)) + geom_line() + geom_point()
(m <- lm(y ~ x, data = df))
summary(m)
(predict.lm(m, newdata = data.frame(x = 20000)))


library(doParallel)
# registerDoParallel(core=4)

system.time(
  for (i in 1:20000){
    for (j in 1:length(unique(Product_A01$PD_M_NM))){
      if (is.null(unlist(Shopping_A01_PD_M_NM_by_ID[ID == i]$PD_M_NM))) {
        User_based_A01[i,j] <- 0
      } else {
        User_based_A01[i,j] <- sum(colnames(User_based_A01)[j] == unlist(Shopping_A01_PD_M_NM_by_ID[ID == i]$PD_M_NM))
      } 
    }
  }
)
View(User_based_A01)

ddply(as.data.frame(Shopping_A01_PD_M_NM_by_ID)[1:10,], "ID", function(x) {
  data.frame(unlist(Shopping_A01_PD_M_NM_by_ID$PD_M_NM))
})

View(as.data.frame(Shopping_A01_PD_M_NM_by_ID)[1:10,])


User_based_A01_100 <- User_based_A01[1:100,]
View(User_based_A01_100)

Cosine_dist_ID <- 1 - as.matrix(dist(User_based_A01_500, method = "cosine"))
View(Cosine_dist_ID)

User_based_cluster <- hclust(as.dist(Cosine_dist_ID), method = "ward")
User_based_cluster
par(mfrow = c(1,2))
plot(User_based_cluster)
plot(User_based_cluster, hang = -1)
par(mfrow = c(1,1))

wssplot <- function(data, nc=100, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of Cluster", 
       ylab = "Within groups sum of squares")
}
wssplot(Cosine_dist_ID)

groups <- cutree(User_based_cluster, k = 20)
plot(User_based_cluster, hang = -1)
rect.hclust(User_based_cluster, k=20, border = "red")

View(User_based_A01_100[c(26,91),])
