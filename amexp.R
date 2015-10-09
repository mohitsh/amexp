library(gdata)
library(nnet)
library(foreign)
library(reshape2)
library(caret)

Traindata <- read.csv("Training_Dataset.csv")
dict_data <- read.xls("Data_Dictionary.xlsx")
leader_data <- read.csv("Leaderboard_Dataset.csv")
y <- Traindata[[2]]
x <- Traindata[3:32]

# model fitting
partyVoted <- x[[1]]                    # party voted for in last election
DonaCent <- as.numeric(x[[2]])        # donation to Centaur
DonaEbo <- as.numeric(x[[3]])        # donation to Ebony
DonaTok <- as.numeric(x[[4]])        # donation to Tokugawa
DonaOdy <- as.numeric(x[[5]])        # donation to Odyssey
DonaCos <- as.numeric(x[[6]])        # donation to Cosmos

ShareCent <- as.numeric(x[[7]])
ShareCent[is.na(ShareCent)] <- mean(ShareCent,na.rm=TRUE)
ShareEbo <- as.numeric(x[[8]])
ShareEbo[is.na(ShareEbo)] <- mean(ShareEbo,na.rm=TRUE)
ShareTok <- as.numeric(x[[9]])
ShareTok[is.na(ShareTok)] <- mean(ShareTok,na.rm=TRUE)
ShareOdy <- as.numeric(x[[10]])
ShareOdy[is.na(ShareOdy)] <- mean(ShareOdy,na.rm=TRUE)
ShareCos <- as.numeric(x[[11]])
ShareCos[is.na(ShareCos)] <- mean(ShareCos,na.rm=TRUE)

Occup <- as.factor(x[[12]])
RegCode <- as.factor(x[[13]])
RegCode <- addNA(RegCode)
HouseHold <- as.numeric(x[[14]])
ageBucket <- as.factor(x[[15]])
married <- as.factor(x[[16]])
HomeOwn <- as.factor(x[[17]])
PolitAffi <- as.numeric(x[[18]])
ResidingYears <- as.numeric(x[[19]])
PerVoted <- as.numeric(x[[20]])
PartiesVoted <- as.numeric(x[[21]])
EducBack <- as.factor(x[[22]])
Rally <- as.numeric(x[[27]])
Rally[is.na(Rally)] <- mean(Rally, na.rm=TRUE)
income <- as.numeric(x[[30]])
income[is.na(income)] <- mean(income,na.rm=TRUE)


train_data = data.frame(y,partyVoted,DonaCent,DonaEbo,DonaTok,DonaOdy,DonaCos,
                        ShareCent,ShareEbo,ShareTok,ShareOdy,ShareCos)

train_data$base <- relevel(train_data$y, ref = "CENTAUR")

fit <- multinom(base~partyVoted+Centaur+Ebony+Tokugawa+Odyssey+Cosmos, data = train_data)

z <- summary(fit)$coefficients/summary(fit)$standard.errors
# two tailed z test
p <- (1-pnorm(abs(z),0,1))*2

# prediction 
x_pred <- leader_data[2:31]
partyVoted <- x_pred[[1]]                    # party voted for in last election
Centaur <- as.numeric(x_pred[[2]])        # donation to Centaur
Ebony <- as.numeric(x_pred[[3]])        # donation to Ebony
Tokugawa <- as.numeric(x_pred[[4]])        # donation to Tokugawa
Odyssey <- as.numeric(x_pred[[5]])        # donation to Odyssey
Cosmos <- as.numeric(x_pred[[6]])        # donation to Cosmos

data = data.frame(partyVoted,Centaur,Ebony,Tokugawa,Odyssey,Cosmos)

prediction <- predict(fit,newdata = data,"probs")
prediction <- as.data.frame(prediction)

# removing odyssesy from response only for final dataset
#prediction <- prediction[c("CENTAUR","COSMOS","EBONY","TOKUGAWA")]

prediction$FinalVote <- colnames(prediction)[apply(prediction,1,which.max)]
write.table(prediction,file="final.csv",sep=",")