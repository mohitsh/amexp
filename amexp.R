library(gdata)
library(nnet)
library(foreign)
library(reshape2)
library(caret)
library(plyr)

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

x$mvar_12 <- as.character(x$mvar_12)
x$mvar_12[x$mvar_12 == "Factory Mager"] <- "Factory Manager"
x$mvar_12[x$mvar_12 == "Senior Magement"] <- "Senior Management"
x$mvar_12[x$mvar_12 == "Middle Magement"] <- "Middle Management"
x$mvar_12 <- as.factor(x$mvar_12)

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

x$mvar_22 <- as.character(x$mvar_22)
x$mvar_22[x$mvar_22 == "professiol"] <- "professional"
x$mvar_22 <- as.factor(x$mvar_22)
RallyCent <- as.numeric(x[[23]])
RallyCent[is.na(RallyCent)] <- mean(RallyCent,na.rm=TRUE)
RallyEbo <- as.numeric(x[[24]])
RallyEbo[is.na(RallyEbo)] <- mean(RallyEbo,na.rm=TRUE)
RallyTok <- as.numeric(x[[25]])
RallyTok[is.na(RallyTok)] <- mean(RallyTok,na.rm=TRUE)
RallyOdy <- as.numeric(x[[26]])
RallyOdy[is.na(RallyOdy)] <- mean(RallyOdy,na.rm=TRUE)
Rally <- as.numeric(x[[27]])
RallyCos <- as.numeric(x[[28]])
RallyCos[is.na(RallyCos)] <- mean(RallyCos,na.rm=TRUE)
Rally[is.na(Rally)] <- mean(Rally, na.rm=TRUE)
income <- as.numeric(x[[30]])
income[is.na(income)] <- mean(income,na.rm=TRUE)


train_data = data.frame(y,partyVoted,DonaCent,DonaEbo,DonaTok,DonaOdy,DonaCos,
                        ShareCent,ShareEbo,ShareTok,ShareOdy,ShareCos,
                        Occup,RegCode,HouseHold,ageBucket,married,HomeOwn,PolitAffi,
                        ResidingYears,PerVoted,PartiesVoted,EducBack,RallyCent,
                        RallyEbo,RallyTok,RallyOdy,Rally,RallyCos,income)

train_data$base <- relevel(train_data$y, ref = "CENTAUR")

#fit <- multinom(base ~ partyVoted+DonaCent+DonaEbo+DonaTok+DonaOdy+DonaCos+
#                ShareCent+ShareEbo+ShareTok+ShareOdy+ShareCos+
#                Occup+RegCode+HouseHold+ageBucket+married+HomeOwn+PolitAffi+
#                ResidingYears+PerVoted+PartiesVoted+EducBack+Rally+income, data = train_data)


fit <- multinom(base ~ partyVoted+DonaCent+DonaEbo+DonaTok+
                DonaOdy+DonaCos+
                ShareCent+ShareEbo+ShareTok+ShareOdy+
                ShareCos+
                        Occup+RegCode+ageBucket+married+HomeOwn+
                        +PerVoted+EducBack+
                        + RallyCent+RallyEbo+RallyTok+RallyOdy+Rally+RallyCos, data = train_data)


#z <- summary(fit)$coefficients/summary(fit)$standard.errors
# two tailed z test
#p <- (1-pnorm(abs(z),0,1))*2

# prediction 
x_pred <- leader_data[2:31]

partyVoted <- x_pred[[1]]                    # party voted for in last election
DonaCent <- as.numeric(x_pred[[2]])        # donation to Centaur
DonaEbo <- as.numeric(x_pred[[3]])        # donation to Ebony
DonaTok <- as.numeric(x_pred[[4]])        # donation to Tokugawa
DonaOdy <- as.numeric(x_pred[[5]])        # donation to Odyssey
DonaCos <- as.numeric(x_pred[[6]])        # donation to Cosmos

ShareCent <- as.numeric(x_pred[[7]])
ShareCent[is.na(ShareCent)] <- mean(ShareCent,na.rm=TRUE)
ShareEbo <- as.numeric(x_pred[[8]])
ShareEbo[is.na(ShareEbo)] <- mean(ShareEbo,na.rm=TRUE)
ShareTok <- as.numeric(x_pred[[9]])
ShareTok[is.na(ShareTok)] <- mean(ShareTok,na.rm=TRUE)
ShareOdy <- as.numeric(x_pred[[10]])
ShareOdy[is.na(ShareOdy)] <- mean(ShareOdy,na.rm=TRUE)
ShareCos <- as.numeric(x_pred[[11]])
ShareCos[is.na(ShareCos)] <- mean(ShareCos,na.rm=TRUE)

Occup <- as.factor(x_pred[[12]])
RegCode <- as.factor(x_pred[[13]])
RegCode <- addNA(RegCode)
HouseHold <- as.numeric(x_pred[[14]])
ageBucket <- as.factor(x_pred[[15]])
married <- as.factor(x_pred[[16]])
HomeOwn <- as.factor(x_pred[[17]])
PolitAffi <- as.numeric(x_pred[[18]])
ResidingYears <- as.numeric(x_pred[[19]])
PerVoted <- as.numeric(x_pred[[20]])
PartiesVoted <- as.numeric(x_pred[[21]])
EducBack <- as.factor(x_pred[[22]])

RallyCent <- as.numeric(x_pred[[23]])
RallyCent[is.na(RallyCent)] <- mean(RallyCent,na.rm=TRUE)
RallyEbo <- as.numeric(x_pred[[24]])
RallyEbo[is.na(RallyEbo)] <- mean(RallyEbo,na.rm=TRUE)
RallyTok <- as.numeric(x_pred[[25]])
RallyTok[is.na(RallyTok)] <- mean(RallyTok,na.rm=TRUE)
RallyOdy <- as.numeric(x_pred[[26]])
RallyOdy[is.na(RallyOdy)] <- mean(RallyOdy,na.rm=TRUE)


Rally <- as.numeric(x_pred[[27]])
Rally[is.na(Rally)] <- mean(Rally, na.rm=TRUE)
RallyCos <- as.numeric(x_pred[[28]])
RallyCos[is.na(RallyCos)] <- mean(RallyCos,na.rm=TRUE)

income <- as.numeric(x_pred[[30]])
income[is.na(income)] <- mean(income,na.rm=TRUE)



#data = data.frame(partyVoted,DonaCent,DonaEbo,DonaTok,
#                  DonaOdy,DonaCos,
#                        ShareCent,ShareEbo,ShareTok,ShareOdy,
#                  ShareCos,
#                        Occup,RegCode,HouseHold,ageBucket,
#                  married,HomeOwn,PolitAffi,
#                        ResidingYears,PerVoted,PartiesVoted,
#                  EducBack,Rally,income)


data = data.frame(partyVoted,DonaCent,DonaEbo,DonaTok,
                  DonaOdy,DonaCos,
                  ShareCent,ShareEbo,ShareTok,ShareOdy,
                  ShareCos,                        
                  Occup,RegCode,ageBucket,
                  married,HomeOwn,
                  PerVoted,
                  EducBack,RallyCent,
                  RallyEbo,RallyTok,RallyOdy,Rally,RallyCos)


prediction <- predict(fit,newdata = data,"probs")

prediction <- as.data.frame(prediction)

# removing odyssesy from response only for final dataset
#prediction <- prediction[c("CENTAUR","COSMOS","EBONY","TOKUGAWA")]

prediction$FinalVote <- colnames(prediction)[apply(prediction,1,which.max)]
write.table(prediction,file="dude7.csv",sep=",")

