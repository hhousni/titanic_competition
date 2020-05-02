library(tidyverse)
library(caret)

train <- read.csv('train.csv')
test <- read.csv ('test.csv')

test$Survived <-NA

data.combined <- bind_rows(train,test)

str(data.combined$Survived) # Show that Survived is coding as INT but this is not the case we need to covert it in a factor 
data.combined$Survived <- as.factor(data.combined$Survived)
str(data.combined$Survived) # show that Survived is now a factor of 2 levels 


##########################################################

#Pclass 

data.combined$Pclass <- as.factor(data.combined$Pclass)
str(data.combined$Pclass) #Show that Pclass is a factor of 3 Levels

#lets take a look of the impact of Pclass in the survived throught visualization

ggplot(data.combined[1:891,], aes(x=Pclass,fill=Survived)) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total_Count") +
  labs(fill="Survived")

#lets creat dummy variable for each class

dmy_Pclass <- dummyVars(~ Pclass, data =data.combined )
data.combined_Pclass <- data.frame(predict(dmy_Pclass,newdata=data.combined))
data.combined_v1 <- cbind(data.combined,data.combined_Pclass) #new train data base with one hote on Plass
rm(dmy_Pclass,data.combined_Pclass) #remove data base we don't need

###################################

#Name 
head(data.combined$Name,20) 
#Show that the feature Name contains mains informations, the most one is the Title of each passenger
#lets create a new varible to separate the title of the others 

data.combined_Title <- data.combined_v1 %>% 
  mutate(Title= as.factor (if_else(str_detect(Name,"Mr"),"Mr",
                           if_else(str_detect(Name,"Miss"), "Miss",
                                   if_else(str_detect(Name, "Mrs"),"Mrs",
                                           if_else(str_detect(Name,"Master"),"Master",
                                                   "Other"))))))
table(data.combined_Title$Title)


#lets take a look if the impact of Title on survived throught Vizualization 

ggplot(data.combined_Title[1:891,], aes(x=Title, fill =Survived))+
  geom_bar() +
  xlab("Title") +
  ylab("Total_Count")+
  labs(fill="Survived")

 dmy_Title <- dummyVars(~ Title, data = data.combined_Title)
 data.combined_Title_1 <- predict(dmy_Title,newdata=data.combined_Title)
 data.combined_v2 <- cbind(data.combined_v1,data.combined_Title_1)

 rm(dmy_Title,data.combined_Title,data.combined_Title_1)

 #################################

#Sex

str(data.combined_v2$Sex)

ggplot(data.combined_v2[1:891,], aes(x=Sex, fill=Survived)) +
  geom_bar()+
  xlab("Sex")+
  ylab("Total_count")+
  labs(fill="Survived")

dmy_sex <- dummyVars(~ Sex, data=data.combined_v2)
dmy_sex_1 <- predict(dmy_sex,newdata=data.combined_v2)
data.combined_v3 <- cbind(data.combined_v2,dmy_sex_1)
rm(dmy_sex,dmy_sex_1)


#############################
#Embarked 
str(data.combined_v3$Embarked)
data.combined_v3$Embarked <- as.factor(data.combined_v3$Embarked)
ggplot(data.combined_v3[1:891,], aes(x=Embarked, fill=Survived))+
  geom_bar() +
  xlab("Embarked") +
  ylab("Total count")
  labs("Survived")
dmy_embraked <- dummyVars(~Embarked, data=data.combined_v3)
dmy_embraked_1 <- predict(dmy_embraked, newdata=data.combined_v3)
data.combined_v4 <- cbind(data.combined_v3,dmy_embraked_1)
rm(dmy_embraked,dmy_embraked_1)
  

#######################################

train_vf <- data.combined_v4[1:891,]
test_vf <- data.combined_v4[892:1309,2:21]


#Prediction 

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
metric <- "Accuracy"

set.seed(12345)
glm1 <- train(Survived ~
                Title.Master +
                Title.Miss +
                Title.Mr +
                Title.Other +
                Sex.female + 
                Sex.male + 
                Pclass.1 +
                Pclass.2 +
                Pclass.3 +
                SibSp +
                Parch +
                Embarked.C +
                Embarked.S,
              data=train_vf, 
              method='glm', 
              metric='Accuracy', 
              trControl=control)


print(glm1)

###########################################


test_df <- as.data.frame(test_vf)
glm.pred.2 <-predict(glm1,test_vf)

table(glm.pred.2)
submit.df <- data.frame(PassengerId=rep(892:1309), Survived=glm.pred.2)
write.csv(submit.df,file ="RF_SUB_20200426.CSV",row.names = FALSE)


