setwd("~/Research/Reinforcement Learning/Next Best Action/")
rm(list=ls())
library(rio)
library(dplyr)
library(ReinforcementLearning)

base=import("Data/santander-product-recommendation/Train/train_ver2.csv")
dict=import("Data/santander-product-recommendation/Data_Dictionary.xlsx")
colnames(base)=as.character(dict$New_Name)
samp=base%>%filter(Customer_ID==1375586)


###Taking a smaller sample of customers
samp=sample_frac(base,0.01)
custs=samp%>%
  mutate(pp=1)%>%
  select(Customer_ID,pp)%>%unique()

data=inner_join(base,custs,by=c("Customer_ID"="Customer_ID"))

##Creating features
data2=data%>%
  mutate(E_Date=as.Date(E_Date)
         ,Start_Date=as.Date(Start_Date))%>%
  mutate(Tenure=as.numeric(E_Date - Start_Date))%>%
  mutate(Tenure_Class=cut(x = Tenure,breaks = c(-100,30,180,365,
                                                720,1095,1460,
                                                1825,3000,10000)
                          ,labels = c('A','B','C','D'
                                      ,'E','F','G','H'
                                      ,'I')))%>%
  select(Customer_ID,E_Date,Age,segmentation,Cust_Relation_Type,Sex
         ,Tenure_Class,Loans,`Credit Card`,Pensions
         ,Loans,Mortgage,e_Account,`Direct Debit`)%>%
  filter(segmentation!='')%>%
  rename(Credit_Card=`Credit Card`
         ,Direct_Debit=`Direct Debit`)


data3=data2[complete.cases(data2),]
chk=data3%>%group_by(Tenure_Class,segmentation)%>%
  summarise_at(vars(Loans:Direct_Debit),sum)

ev=data3%>%
  summarise_at(vars(Loans:Direct_Debit),sum)
ev$Tot=rowSums(ev)


ev[,1:6]=ev[,1:6]/ev[,7]
lookup=data.frame(colnames(ev[1:6]))
colnames(lookup)=c('cols')
lookup$ID=seq.int(1,6,1)
lookup$ID=as.character(lookup$ID)

##Crate Reward for activity
data4=data3%>%
  mutate(Loans_Rew=if_else(Loans>0,200,-5)
           ,Credit_Card_Rew=if_else(Credit_Card>0,100,-15)
         ,Pensions_Rew=if_else(Pensions>0,50,-15)
         ,Mortgage_Rew=if_else(Mortgage>0,150,-10)
         ,e_Account_Rew=if_else(e_Account>0,50,-20)
         ,Direct_Debit_Rew=if_else(Direct_Debit>0,40,-20))


###
seq=sample(seq.int(from = 1,to = 6,by = 1),size = nrow(data3), replace = TRUE)
df=data.frame((seq))
df$X.seq.=as.character(df$X.seq.)

df2=inner_join(df,lookup,by=c("X.seq."="ID"))


data4$Action=df2$cols
data5=data4%>%
      mutate(Reward=if_else(Action=='Loan',Loans_Rew
                            ,if_else(Action=='Credit_Card',Credit_Card_Rew
                                     ,if_else(Action=='Pensions',Pensions_Rew
                                              ,if_else(Action=='Mortgage',Mortgage_Rew
                                                       ,if_else(Action=='e_Account',e_Account_Rew,Direct_Debit_Rew))))))%>%
  mutate(State=paste(Tenure_Class,segmentation))%>%
  ungroup()%>%
  group_by(Customer_ID)%>%
  arrange(E_Date,Customer_ID)%>%
  mutate(Next_State=lead(State))%>%
  ungroup()%>%
  select(Customer_ID,E_Date,State,Action,Reward,Next_State)



data6=data5%>%filter(is.na(Next_State)==0)%>%
  mutate(State=as.character(State)
         ,Action=as.character(Action)
         ,Reward=as.numeric(Reward)
         ,Next_State=as.character(Next_State))

# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)

samp=data6%>%head(100000)
colnames(samp)
# Perform reinforcement learning
model <- ReinforcementLearning(data6, 
                               s = "State", 
                               a = "Action", 
                               r = "Reward", 
                               s_new = "Next_State", 
                               control = control,verbose = TRUE)

Q=model$Q


###Continue Learning from these
model_Update <- ReinforcementLearning(data6%>%head(10000), 
                               s = "State", 
                               a = "Action", 
                               r = "Reward", 
                               s_new = "Next_State", 
                               control = control,verbose = TRUE
                               ,model = model,iter = 10)

Q_Update=model_Update$Q

Q_Tab=data.frame(Q_Update)
Q_Tab$State=row.names(Q_Tab)
library(tidyr)

Q_Tab_Long=Q_Tab%>%gather(key = Action,value = Value,Loans:e_Account)


# Calculate optimal policy
pol <- computePolicy(model_Update)

policy=data.frame(pol)
policy$State=row.names(policy)
