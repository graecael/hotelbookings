#assessing the packages
library(data.table)
library(caTools)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(GGally)
library (corrplot)
library (car)
library (VIM)
library (tidyverse)
library(dplyr)


#Set Working Directory
setwd("C:/Users/grace/OneDrive - Nanyang Technological University/AY2223 Sem 2/BC2407 Analytics II/Project")

#naming second dataset
hb.dt <- fread("hotel_bookings.csv", stringsAsFactors = T, header=TRUE)

#based on the research paper and our own interpretation, we have categorise some of the columns
summary(hb.dt)
sapply(hb.dt,"class")
hb.dt$is_canceled<-factor(hb.dt$is_canceled)
hb.dt$is_repeated_guest<-factor(hb.dt$is_repeated_guest)

#-------------------------------------------------------------------------------

#Next, we will be removing unnecessary data columns. We can concatenate arrival_date_year, arrival_date_month,arrival_date_day_of_month to arrival_date and drop the columns. This will maintain consistency as reservation_status_date which is on IDate format.
levels(hb.dt$arrival_date_month)
hb.dt$arrival_month<-hb.dt$arrival_date_month
hb.dt$arrival_month <- as.character(hb.dt$arrival_month)
hb.dt$arrival_month[hb.dt$arrival_month=="January"]<-"1"
hb.dt$arrival_month[hb.dt$arrival_month=="February"]<-"2"
hb.dt$arrival_month[hb.dt$arrival_month=="March"]<-"3"
hb.dt$arrival_month[hb.dt$arrival_month=="April"]<-"4"
hb.dt$arrival_month[hb.dt$arrival_month=="May"]<-"5"
hb.dt$arrival_month[hb.dt$arrival_month=="June"]<-"6"
hb.dt$arrival_month[hb.dt$arrival_month=="July"]<-"7"
hb.dt$arrival_month[hb.dt$arrival_month=="August"]<-"8"
hb.dt$arrival_month[hb.dt$arrival_month=="September"]<-"9"
hb.dt$arrival_month[hb.dt$arrival_month=="October"]<-"10"
hb.dt$arrival_month[hb.dt$arrival_month=="November"]<-"11"
hb.dt$arrival_month[hb.dt$arrival_month=="December"]<-"12"
hb.dt$arrival_date <- paste(hb.dt$arrival_date_year, hb.dt$arrival_month,hb.dt$arrival_date_day_of_month,sep="-")
hb.dt$arrival_date <- as.IDate(hb.dt$arrival_date, format="%Y-%m-%d" )



#We will drop the agent and company as it will not be relevant to us as they are encoded with numbers due to data privacy policy. We do not have the type of industry of company and will not form any significant conclusion to the data. Hence the team decided to drop the 2 columns. We have also drop the original columns arrival_date_year, arrival_date_month, arrival_date_day_of_month, arrival_month
hb.dt <- hb.dt[, ! c("agent","company","arrival_date_year","arrival_date_month","arrival_date_day_of_month","arrival_month")]
hb.dt <- hb.dt[, ! c("arrival_date_week_number","reservation_status_date","arrival_date")]
hb.dt <- hb.dt[, ! c("country")]
hb.dt <- hb.dt[, ! c("reservation_status")]
#-------------------------------------------------------------------------------

# ========================================================Data Exploration ===============================================================
dim(hb.dt) #119390 rows and 28 columns
summary(hb.dt)
colnames(hb.dt)

#1:looking at the summary, the children column has 4 NA cases, these 4 NA cases constitute an insignificant percentage in the whole dataset of 0.003%, hence, the team verdict to remove the 4 cases with NA value.
sum(is.na(hb.dt$children))/nrow(hb.dt)*100
hb.dt <-hb.dt[complete.cases(hb.dt$children), ] 

#2: looking at the summary, for the column meal plan undefined, it is one of the categorical factor which means the customers did not order any meal plan package, hence, the column will remained as it is.

#3: After cleaning the children column, the distribution_channel column has 1 undefined values. A drop of 4 undefined value previously identified in the summary. It is not part of the categorical factor and hence, the team plans to drop the 1 cases since the 1 case constitute a small factor in the dataset of 0.0008%
summary(hb.dt)
sum(hb.dt$distribution_channel=="Undefined")/nrow(hb.dt)*100
hb.dt<- hb.dt[hb.dt$distribution_channel!="Undefined"]

#4: ADR is determined as average daily rate which means it is calculated by dividing the sum of all lodging transactions by the total number of night stayed. Based on the visualization graph, we have found that there are some rows with negative lodging transaction. It is impossible for ADR to be negative as there ought to be some transactions and even if there are refunds, hotels would not provide refunds that is beyond what customers are paid off. Thus, the team have done some data visualization to determine whether the rows should be dropped or be replaced with a positive value
summary(hb.dt)
## Graphic visualization of ADR
ggplot(data = hb.dt, aes(x = adr)) + 
  geom_boxplot(fill="yellow") +
  labs(title = "Box plot of ADR", y = "ADR") +
  theme_bw()
## From the graph there is an extreme outlier of adr at $5400, we will retrieve the row to figure why is it so and determine whether the outlier should be removed.
outlier<-hb.dt[hb.dt$adr==5400,]
##when we retrieve the column, we realised that the customer has a non refundable transaction with a grade A room and customers had cancelled the reservation. Hence, the charge may be reasonable and the team has decided not to drop the outlier. Nonetheless, the team has remove the row with negative adr as there is only one row with negative adr.
sum(hb.dt$adr<0)
hb.dt<- hb.dt[hb.dt$adr>=0]

#We have cleaned the data.

write.csv(hb.dt, file="clean_hotel_booking.csv")

# =======================================================Data Visualisation ===============================================================

# Test for Multicollinearity 
ivar <- hb.dt [, c(3,4,5,6,7,8,9,15,16,19,21,23,24,25)]
ivar <- ivar[complete.cases(ivar)]
corrplot (corr = cor (ivar), order = "AOE", method = "number",tl.cex = 0.5)
#no columns with major collinearity.

#analyzing the collinearity for categorical variables
icatvar <-  hb.dt [, c(1,2,10,12,13,14,17,18,20,22)]
icatvar <- icatvar[complete.cases(icatvar)]
library(ggcorrplot)
model.matrix(~0+., data=icatvar) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2, tl.cex = 0.5)

#-----------------------------------------------------------------------------------

#We will be retrieving cases that is city hotel since we will be implementing our methodology in Singapore's context and Singapore is a city thus we will be looking at city hotel only.
#we will be subsetting the hotel to "city hotel" after cleaning the original dataset so we will be able to use the original dataset when required
#sum of city hotel is 79330 which is still alot
sum(hb.dt$hotel=="City Hotel")
hb2.dt<-hb.dt[hb.dt$hotel=="City Hotel",]
sum(hb2.dt$is_canceled=="1")


counts <- lapply(colnames(hb.dt), function(col) {
    hb.dt %>% count(!!sym(col))
  })

counts[2]

# Target Variable: Cancelled (0 = No, 1 = Yes)
ggplot(data = hb.dt) +
  geom_bar(mapping = aes(x = is_canceled), stat = "count") +
  labs(title = "Number of Cancellations", x = "Cancelled", y = "Count") 

#Class Imbalance of Target Variable:44224 cancellations compared to 75166 non-cancellations


# LOGISTIC REGRESSION ==========================================================
library(ROSE)

# Check class imbalance in the data
table(hb.dt$is_canceled)


# 70/30 Train-test split =========================================================
round(prop.table(table(hb.dt$is_canceled)),3)
train <- sample.split(Y = hb.dt$is_canceled, SplitRatio = 0.7)
trainset <- subset(hb.dt, train == T)
testset <- subset(hb.dt, train == F)
threshold1 <- 0.5

# log1 Logistic Regression with everything =======================================
log <- glm(is_canceled ~ . , family = binomial, data = trainset)
summary(log)    

OR <- exp(coef(log))
OR

OR.CI <- exp(confint(log))
OR.CI

# Confusion Matrix on Trainset
prob.train.log<- predict(log, type = 'response')
log.new.predict.train <- ifelse(prob.train.log > threshold1, "Cancelled", "Not Cancelled")
table1 <- table(Trainset.Actual = trainset$is_canceled, log.new.predict.train)
table1
table1 <- round(prop.table(table1),3)
accuracy <- 1- sum(diag(table1))
accuracy


# Confusion Matrix on Testset
prob.test.log.new <- predict(log, newdata = testset, type = 'response')
log.new.predict.test <- ifelse(prob.test.log.new > threshold1, "Cancelled", "Not Cancelled")
table2 <- table(log.new.predict.test, Testset.Actual = testset$is_canceled)
table2
table2 <- round(prop.table(table2),3)
accuracy <- 1- sum(diag(table2))
accuracy


# CART =========================================================================
library(caTools)
library(rpart)
library(rpart.plot)

train <- sample.split(Y = hb.dt$is_canceled, SplitRatio = 0.7)
trainset <- subset(hb.dt, train == T)
testset <- subset(hb.dt, train == F)

#10 Fold Cross Validation using RPart
set.seed(1)
cart <- rpart(is_canceled ~ ., data = hb.dt, method = 'class',
              control = rpart.control(minsplit = 2, cp = 0))
rpart.plot(cart, nn= T, main = "Maximal Tree for Classification")
print(cart)
printcp(cart) #Root node error: 44220/119384 = 0.3704
plotcp(cart, main = "Size of Tree")

# Plot variable importance
imp <- as.data.frame(varImp(cart))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

#Pruning 
cp1 <- sqrt(0.00000004)
cartprune <- prune(cart, cp=cp1)
print(cartprune)
printcp(cartprune)
plotcp(cartprune, main = "Size of Tree")
rpart.plot(cartprune, nn= T, main = "Pruned Tree")

#Train Test
cart_train <- rpart(is_canceled~ ., data = trainset, method = 'class',
                    control = rpart.control(minsplit = 2, cp = 0))
rpart.plot(cart_train, nn= T, main = "Maximal Tree in Train Set")
print(cart_train)
printcp(cart_train)
plotcp(cart_train, main = "Subtrees in Test Set")

#Pruning Train Set CART
cp1_train <- sqrt(0.000000005)
cartprune_train <- prune(cart_train, cp=cp1_train)
print(cartprune_train)
printcp(cartprune_train)
plotcp(cartprune_train, main = "Size of Tree")
rpart.plot(cartprune_train, nn= T, main = "Pruned Train Tree")

#Train Set Prediction
threshold1 <- 0.5
prob.train <- predict(cartprune_train, type = 'class')
cartprune_traintable <- table(Trainset.Actual = trainset$diagnosis, prob.train)
cartprune_traintable
mean(prob.train == trainset$diagnosis) #Accuracy of train set

#Test Set Prediction
prob.test <- predict(cartprune_train, newdata = testset, type = 'class')
cartprune_testtable <- table(Testset.Actual = testset$diagnosis, prob.test)
cartprune_testtable
mean(prob.test == testset$diagnosis) #Accuracy of test set

FPR <- cartprune_testtable[2,1]/ (cartprune_testtable[2,1]+cartprune_testtable[1,1])
FPR 
FNR <- cartprune_testtable[1,2]/ (cartprune_testtable[1,2]+cartprune_testtable[2,2])
FNR 

