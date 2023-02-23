library(dplyr)
library(tidyr)
library(stringr)
library(VIM)
library(tidyverse)
library(plotly)
library(factoextra)
library(CatEncoders)
library(corrplot)

#Import Data
df <- read.csv("raw_data.csv")


#Duplicate Data
df <- df %>% distinct(Name, .keep_all = TRUE)

#df1 <- as.data.frame(do.call(rbind, strsplit(as.character(df$Type_of_Loan), ", ")))


#Select Atribut

#-------------------------------------------------------------------------------

#Format tipe data salah
summary(df) #Cek class tipe data pada setiap atribut
#View(select_if(df,is.character)) 
  #Atribut yg harusnya tipe data numeric : Age, Annual_Income, Num_of_Loan, Num_of_Delayed_Payment, Changed_Credit_limit, Outstanding_Debt,Payment_Behaviour,Monthly_balance,Credit_History_Age
  #Karena tidak terbaca numeric berarti ada salah input sehingga datanya terindikasi string

#Check Atribut String/Character Unique
sapply(subset(df,select = c(Month,Occupation,Credit_Mix,Payment_of_Min_Amount,Payment_Behaviour)), unique)
  #Atribut yg tidak ada salah input hanya Month, selain atribut itu terindikasi salah input
  #Atribut Occupation : "_______"
  #Credit_Mix : "_"
  #Payment_of_Min_Amount : "NM"
  #Payment_Behaviour : "!@9#%8"

#Pembersihan salah input pada atribut string
df <- df[!(df$Occupation=="_______"|df$Credit_Mix== "_"|df$Payment_of_Min_Amount == "NM"|df$Payment_Behaviour=="!@9#%8"|df$SSN == '#F%$D@*&8'),]

#Mengatasi Specific input Data dan convert numerik

#Age
df$Age <- as.numeric(gsub('[_]','',df$Age))

#Annual_Income
df$Annual_Income <- as.numeric(gsub('[_]','',df$Annual_Income))

#Num_of_Loan
df$Num_of_Loan <- as.numeric(gsub('[_]','',df$Num_of_Loan))

#Num_of_Delayed_Payment
df$Num_of_Delayed_Payment <- as.numeric(gsub('[_]','',df$Num_of_Delayed_Payment))

#Outstanding_Debt
df$Outstanding_Debt <- as.numeric(gsub('[_]','',df$Outstanding_Debt))

#Monthly_balance
df$Monthly_Balance <- as.numeric(gsub('[_]','',df$Monthly_Balance))

#Changed_Credit_Limit
df$Changed_Credit_Limit <- as.numeric(gsub('[_]','',df$Changed_Credit_Limit))

#Amount
df$Amount_invested_monthly <- as.numeric(gsub('[_]','',df$Amount_invested_monthly))

#-------------------------------------------------------------------------------

#Transformasi Data

#Agregasi
#Credit_History_Age
df$Credit_History_Age <- gsub('[ears and Months]','',df$Credit_History_Age)
df[,28:29] <- str_split_fixed(df$Credit_History_Age,"Y",2)

df$Credit_History_Age <- (as.numeric(df$V28)*12)+(as.numeric(df$V29))

df <- df[,-c(28:29)]

#Pembersihan Outlier
df <- subset(df,0 < Num_of_Loan & Num_Bank_Accounts > 0 & Age > 5 & Age < 100)

#Label Encoding
#Month
labs_month = LabelEncoder.fit((df$Month))
df$Month <- transform(labs_month,df$Month)

#Occupation
labs_occ = LabelEncoder.fit((df$Occupation))
df$Occupation <- transform(labs_occ,df$Occupation)

#Credit_Mix
labs_credit = LabelEncoder.fit((df$Credit_Mix))
df$Credit_Mix <- transform(labs_credit,df$Credit_Mix)

#Payment_of_Min_Amount
labs_pay = LabelEncoder.fit((df$Payment_of_Min_Amount))
df$Payment_of_Min_Amount <- transform(labs_pay,df$Payment_of_Min_Amount)

#Payment_Behaviour
labs_behaviour = LabelEncoder.fit((df$Payment_Behaviour))
df$Payment_Behaviour <- transform(labs_behaviour,df$Payment_Behaviour)

# === Visualisasi Statistika Deskriptif ===
df1 <- subset(df,select = -c(Customer_ID,Name,ID,SSN,Type_of_Loan))

aggr_plot <- aggr(df1, col=c('navyblue','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(data), cex.axis=.7,
                  gap=3, ylab=c("Histogram of missing data","Pattern"))

par(mfrow=c(4,5))
for(i in 4:22) {
  plot(hist(df1[,i]), main=names(df1)[i])
}


#Imputasi Missing Value
df1 <- df1 %>%
  mutate_all(funs(ifelse(is.na(.), median(., na.rm = TRUE), .)))

#Imputasi Outlier
df1 <- df1 %>%
  mutate_all(funs(ifelse(abs(.) > 3 * IQR(.) + quantile(., type = 0.75), median(., na.rm = TRUE), .)))

# === Normalisasi Min-Max ===
#numdf <- df1[, sapply(df1, is.numeric)]
df1 <- apply(df1,2, function(x) (x - min(x)) / (max(x) - min(x)))






#===== PCA =====

# Perform PCA on the dataset
varx <- subset(df1,select = -c(Monthly_Balance))
pca <- prcomp(varx, scale. = TRUE)

# Extract the principal components
pcs <- pca$x

# Calculate the proportion of variance explained by each principal component
variance <- pca$sdev^2
prop_var <- variance/sum(variance)

# Visualize the proportion of variance explained by each principal component
prop_var_df <- data.frame(PC = paste0("PC", 1:length(prop_var)), Prop_Var = prop_var)
plot <- ggplot(prop_var_df, aes(reorder(PC,-Prop_Var,sum),Prop_Var)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Proportion of Variance Explained by Principal Components")
  
ggplotly(plot)

#Penggabungan Data Bersih
vary <- subset(df1, select = c(Monthly_Balance))
pcs1 <- subset(pcs,select = c(PC1,PC2,PC3,PC4,PC5))
clean <- cbind(pcs1,vary)

write.csv(clean,'clean_model.csv')

