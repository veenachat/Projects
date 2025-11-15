#data pre processing
rm(list=ls())


install.packages("stringr")
library(dplyr)
library(stringr)


#Set working directory
setwd("/Users/veenachatterjee/Desktop/Tilburg_SEM2/DSM_hw2")

#read data
data <- read.csv("SBAnational.csv")
sum(is.na(data)) #checking missing values
rows_with_na <- which(rowSums(is.na(data)) > 0) #which row number have missing data
data <- data[-rows_with_na, ] #remove NA rows from data

# data%>%summary()

#remove $ signs and , from the numbers
data$SBA_Appv<-str_replace_all(data$SBA_Appv,"[\\$,]","") %>% as.double()
data$GrAppv<-str_replace_all(data$GrAppv,"[\\$,]","") %>% as.double()
data$ChgOffPrinGr<-str_replace_all(data$ChgOffPrinGr,"[\\$,]","") %>% as.double()
data$BalanceGross<-str_replace_all(data$BalanceGross,"[\\$,]","") %>% as.double()
data$DisbursementGross<-str_replace_all(data$DisbursementGross,"[\\$,]","") %>% as.double()

# #replace problem where there was a A behind the year numbers
# data$ApprovalFY%>%unique()
# data$ApprovalFY<-str_replace_all(data$ApprovalFY,"A","")  %>% as.integer()

#data$NewExist can take values 1 or 2, however there are also false values with 0 reported. We remove those rows
data<-data%>%filter(!NewExist==0)

set.seed(123) 
train = sample(dim(data)[1], 1000) #training sample rng numbers
data.train = data[train,] #defining training sample
test = data[-train,] #remaining data we can use for test set
data.test=sample(dim(test)[1], 100000) #defining test set with rng numbers


##LOGISTIC REGRESSION

columns_to_drop <- c("Name", "City", "State", "LoanNr_ChkDgt", "Zip","Bank", "BankState", 
                     "ApprovalDate" , "ApprovalFY" , "FranchiseCode", "DisbursementDate" , 
                     "ChgOffDate" )

# Dropping columns
for(col in columns_to_drop) {
  data <- subset(data, select = -which(names(data) == col))
}

#keeping only first 2 digits of NCAIS columns
data[,1] <- as.numeric(substr(as.character(data[,1]), 1, 2))

data$MIS_Status <- ifelse(data$MIS_Status == "CHGOFF", 1, 0)
data$RevLineCr <- ifelse(data$RevLineCr == "Y", 1, 0)
data$LowDoc <- ifelse(data$LowDoc == "Y", 1, 0)

#factor all categorical values
data$NAICS =factor(data$NAICS)
data$Term   =factor(data.train$Term)       
data$NoEmp=factor(data$NoEmp)
data$NewExist=factor(data$NewExist)
data$CreateJob =factor(data$CreateJob)
data$RetainedJob=factor(data$RetainedJob)      
data$UrbanRural=factor(data$UrbanRural)       
data$RevLineCr =factor(data$RevLineCr)      
data$LowDoc  =factor(data$LowDoc)         
data$DisbursementGross =factor(data$DisbursementGross )
data$BalanceGross=factor(data$BalanceGross)     
data$ChgOffPrinGr =factor(data$ChgOffPrinGr)   
data$GrAppv =factor(data$GrAppv)        
data$SBA_Appv =factor(data$SBA_Appv)      
data.train$MIS_Status  =data.train$MIS_Status=="CHGOFF"

model = glm(MIS_Status ~ NAICS  + Term + NoEmp + NewExist + CreateJob +
              RetainedJob +UrbanRural + RevLineCr + LowDoc
              +DisbursementGross +ChgOffPrinGr
            + GrAppv + SBA_Appv, data = data.train, family = binomial)

