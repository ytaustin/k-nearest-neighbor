library(tidyverse)
library(caret)
data(GermanCredit)
GermanCredit <- as.tibble(GermanCredit)
GermanCredit <-
  select(
    GermanCredit,
    Duration,
    Amount,
    InstallmentRatePercentage,
    Age,
    Class,
    CheckingAccountStatus.lt.0,
    CheckingAccountStatus.0.to.200,
    CheckingAccountStatus.gt.200,
    CheckingAccountStatus.none,
    CreditHistory.NoCredit.AllPaid,
    CreditHistory.ThisBank.AllPaid,
    CreditHistory.PaidDuly,
    CreditHistory.Delay,
    CreditHistory.Critical,
    SavingsAccountBonds.lt.100,
    SavingsAccountBonds.100.to.500,
    SavingsAccountBonds.500.to.1000,
    SavingsAccountBonds.gt.1000,
    SavingsAccountBonds.Unknown,
    EmploymentDuration.lt.1,
    EmploymentDuration.1.to.4,
    EmploymentDuration.4.to.7,
    EmploymentDuration.gt.7,
    EmploymentDuration.Unemployed,
    Personal.Male.Divorced.Seperated,
    Personal.Female.NotSingle,
    Personal.Male.Single,
    Personal.Male.Married.Widowed,
    Personal.Female.Single,
    Housing.Rent,
    Housing.Own,
    Housing.ForFree
  )

ggplot(data = GermanCredit)+
  geom_boxplot(mapping = aes(x = Class, y = Duration))
ggsave("duration.jpg")

ggplot(data = GermanCredit)+
  geom_boxplot(mapping = aes(x = Class, y = Amount))
ggsave("Amount.jpg")

ggplot(data = GermanCredit)+
  geom_boxplot(mapping = aes(x = Class, y = InstallmentRatePercentage))
ggsave("InstallmentRatePercentage.jpg")

ggplot(data = GermanCredit)+
  geom_boxplot(mapping = aes(x = Class, y = Age))
ggsave("Age.jpg")

GermanCredit <- mutate(GermanCredit, 
                       CheckingAccountStatus = 
                         as.factor(ifelse(GermanCredit$CheckingAccountStatus.lt.0 > 0, "lt.0", 
                                   ifelse(GermanCredit$CheckingAccountStatus.0.to.200 > 0, "0.to.200",
                                   ifelse(GermanCredit$CheckingAccountStatus.gt.200 > 0, "gt.200", 
                                   ifelse(GermanCredit$CheckingAccountStatus.none > 0, "none","none"))))))
ggplot(data = GermanCredit, mapping = aes(x = CheckingAccountStatus, y = Class)) + geom_jitter()
ggsave("CheckingAccountStatus.jpg")

GermanCredit <- mutate(GermanCredit,
                       CreditHistory = 
                         as.factor(ifelse(GermanCredit$CreditHistory.Critical > 0, "Critical", 
                                   ifelse(GermanCredit$CreditHistory.Delay > 0, "Delay", 
                                   ifelse(GermanCredit$CreditHistory.NoCredit.AllPaid > 0, "NoCredit_AllPaid", 
                                   ifelse(GermanCredit$CreditHistory.PaidDuly > 0, "PaidDuly",
                                   ifelse(GermanCredit$CreditHistory.ThisBank.AllPaid > 0, "ThisBank_AllPaid", "none")))))))
ggplot(data = GermanCredit, mapping = aes(x = CreditHistory, y = Class)) + geom_jitter()
ggsave("CreditHistory.jpg")

GermanCredit <- mutate(GermanCredit,
                       SavingsAccountBonds = 
                         as.factor(ifelse(GermanCredit$SavingsAccountBonds.lt.100 > 0, "lt.100", 
                                   ifelse(GermanCredit$SavingsAccountBonds.100.to.500 > 0, "100.to.500", 
                                   ifelse(GermanCredit$SavingsAccountBonds.500.to.1000 > 0, "500.to.1000", 
                                   ifelse(GermanCredit$SavingsAccountBonds.gt.1000 > 0, "gt.1000",
                                   ifelse(GermanCredit$SavingsAccountBonds.Unknown > 0, "Unknown", "none")))))))
ggplot(data = GermanCredit, mapping = aes(x = SavingsAccountBonds, y = Class)) + geom_jitter()
ggsave("SavingsAccountBonds.jpg")

GermanCredit <- mutate(GermanCredit,
                       EmploymentDuration = 
                         as.factor(ifelse(GermanCredit$EmploymentDuration.lt.1 > 0, "lt.1", 
                                   ifelse(GermanCredit$EmploymentDuration.1.to.4 > 0, "1.to.4", 
                                   ifelse(GermanCredit$EmploymentDuration.4.to.7 > 0, "4.to.7", 
                                   ifelse(GermanCredit$EmploymentDuration.gt.7 > 0, "gt.7",
                                   ifelse(GermanCredit$EmploymentDuration.Unemployed > 0, "Unemployed", "none")))))))
ggplot(data = GermanCredit, mapping = aes(x = EmploymentDuration, y = Class)) + geom_jitter()
ggsave("EmploymentDuration.jpg")

GermanCredit <- mutate(GermanCredit,
                       Personal = 
                         as.factor(ifelse(GermanCredit$Personal.Male.Divorced.Seperated > 0, "Male.Divorced.Seperated", 
                                   ifelse(GermanCredit$Personal.Female.NotSingle > 0, "Female.NotSingle", 
                                   ifelse(GermanCredit$Personal.Male.Single > 0, "Male.Single", 
                                   ifelse(GermanCredit$Personal.Male.Married.Widowed > 0, "Male.Married.Widowed",
                                   ifelse(GermanCredit$Personal.Female.Single > 0, "Female.Single", "none")))))))
ggplot(data = GermanCredit, mapping = aes(x = Personal, y = Class)) + geom_jitter()
ggsave("Personal.jpg")

GermanCredit <- mutate(GermanCredit,
                       Housing = 
                         as.factor(ifelse(GermanCredit$Housing.Rent > 0, "Rent", 
                                   ifelse(GermanCredit$Housing.Own > 0, "Own",
                                   ifelse(GermanCredit$Housing.ForFree > 0, "ForFree", "none")))))
ggplot(data = GermanCredit, mapping = aes(x = Housing, y = Class)) + geom_jitter()
ggsave("Housing.jpg")


set.seed(121)
trainIndex <-
  createDataPartition(GermanCredit$Class,
                      p = 0.8,
                      list = FALSE,
                      times = 1)
germanTrain <- GermanCredit[trainIndex, ]
germanTest <- GermanCredit[-trainIndex, ]
scaler <- preProcess(germanTrain, method = c("center", "scale"))
germanTrain <- predict(scaler, germanTrain)
germanTest <- predict(scaler, germanTest)

knnmodel_Duration <-
  train(Class ~ Duration, data = germanTrain, method = "knn")
germanTestPredictions_Duration <-
  predict(knnmodel_Duration, germanTest)
confusionMatrix(germanTestPredictions_Duration, germanTest$Class)

knnmodel_Amount <-
  train(Class ~ Amount, data = germanTrain, method = "knn")
germanTestPredictions_Amount <-
  predict(knnmodel_Amount, germanTest)
confusionMatrix(germanTestPredictions_Amount, germanTest$Class)

knnmodel_InstallmentRatePercentage <-
  train(Class ~ InstallmentRatePercentage,
        data = germanTrain,
        method = "knn")
germanTestPredictions_InstallmentRatePercentage <-
  predict(knnmodel_InstallmentRatePercentage, germanTest)
confusionMatrix(germanTestPredictions_InstallmentRatePercentage,
                germanTest$Class)

knnmodel_Age <-
  train(Class ~ Age, data = germanTrain, method = "knn")
germanTestPredictions_Age <- predict(knnmodel_Age, germanTest)
confusionMatrix(germanTestPredictions_Age, germanTest$Class)

knnmodel_CheckingAccountStatus <-
  train(
    Class ~ CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CheckingAccountStatus <-
  predict(knnmodel_CheckingAccountStatus, germanTest)
confusionMatrix(germanTestPredictions_CheckingAccountStatus,
                germanTest$Class)

knnmodel_CreditHistory <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory <-
  predict(knnmodel_CreditHistory, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory, germanTest$Class)

knnmodel_SavingsAccountBonds <-
  train(
    Class ~ SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_SavingsAccountBonds <-
  predict(knnmodel_SavingsAccountBonds, germanTest)
confusionMatrix(germanTestPredictions_SavingsAccountBonds, germanTest$Class)

knnmodel_EmploymentDuration <-
  train(
    Class ~ EmploymentDuration.lt.1 +
      EmploymentDuration.1.to.4 +
      EmploymentDuration.4.to.7 +
      EmploymentDuration.gt.7 +
      EmploymentDuration.Unemployed,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_EmploymentDuration <-
  predict(knnmodel_EmploymentDuration, germanTest)
confusionMatrix(germanTestPredictions_EmploymentDuration, germanTest$Class)

knnmodel_Personal <- train(
  Class ~ Personal.Male.Divorced.Seperated +
    Personal.Female.NotSingle +
    Personal.Male.Single +
    Personal.Male.Married.Widowed +
    Personal.Female.Single,
  data = germanTrain,
  method = "knn"
)
germanTestPredictions_Personal <-
  predict(knnmodel_Personal, germanTest)
confusionMatrix(germanTestPredictions_Personal, germanTest$Class)

knnmodel_Housing <- train(
  Class ~ Housing.Rent +
    Housing.Own +
    Housing.ForFree,
  data = germanTrain,
  method = "knn"
)
germanTestPredictions_Housing <-
  predict(knnmodel_Housing, germanTest)
confusionMatrix(germanTestPredictions_Housing, germanTest$Class)



#round 2


knnmodel_CreditHistory_Duration <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      Duration,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_Duration <-
  predict(knnmodel_CreditHistory_Duration, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_Duration, germanTest$Class)

knnmodel_CreditHistory_Amount <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      Amount,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_Amount <-
  predict(knnmodel_CreditHistory_Amount, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_Amount, germanTest$Class)

knnmodel_CreditHistory_InstallmentRatePercentage <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      InstallmentRatePercentage,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_InstallmentRatePercentage <-
  predict(knnmodel_CreditHistory_InstallmentRatePercentage, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_InstallmentRatePercentage, germanTest$Class)

knnmodel_CreditHistory_Age <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      Age,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_Age <-
  predict(knnmodel_CreditHistory_Age, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_Age, germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus, germanTest$Class)

knnmodel_CreditHistory_SavingsAccountBonds <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_SavingsAccountBonds <-
  predict(knnmodel_CreditHistory_SavingsAccountBonds, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_SavingsAccountBonds, germanTest$Class)

knnmodel_CreditHistory_EmploymentDuration <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      EmploymentDuration.lt.1 +
      EmploymentDuration.1.to.4 +
      EmploymentDuration.4.to.7 +
      EmploymentDuration.gt.7 +
      EmploymentDuration.Unemployed,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_EmploymentDuration <-
  predict(knnmodel_CreditHistory_EmploymentDuration, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_EmploymentDuration, germanTest$Class)

knnmodel_CreditHistory_Personal <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      Personal.Male.Divorced.Seperated +
      Personal.Female.NotSingle +
      Personal.Male.Single +
      Personal.Male.Married.Widowed +
      Personal.Female.Single,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_Personal <-
  predict(knnmodel_CreditHistory_Personal, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_Personal, germanTest$Class)

knnmodel_CreditHistory_Housing <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      Housing.Rent +
      Housing.Own +
      Housing.ForFree,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_Housing <-
  predict(knnmodel_CreditHistory_Housing, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_Housing, germanTest$Class)


#Round 3

knnmodel_CreditHistory_CheckingAccountStatus_Duration <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      Duration,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_Duration <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_Duration, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_Duration, germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_Amount <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      Amount,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_Amount <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_Amount, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_Amount, germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_InstallmentRatePercentage <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      InstallmentRatePercentage,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_InstallmentRatePercentage <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_InstallmentRatePercentage, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_InstallmentRatePercentage, germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_Age <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      Age ,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_Age  <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_Age , germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_Age , germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds, germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_EmploymentDuration<-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      EmploymentDuration.lt.1 +
      EmploymentDuration.1.to.4 +
      EmploymentDuration.4.to.7 +
      EmploymentDuration.gt.7 +
      EmploymentDuration.Unemployed,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_EmploymentDuration <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_EmploymentDuration, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_EmploymentDuration, germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_Personal<-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      Personal.Male.Divorced.Seperated +
      Personal.Female.NotSingle +
      Personal.Male.Single +
      Personal.Male.Married.Widowed +
      Personal.Female.Single,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_Personal <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_Personal, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_Personal, germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_Housing<-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      Housing.Rent +
      Housing.Own +
      Housing.ForFree,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_Housing <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_Housing, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_Housing, germanTest$Class)

#Round 4

knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Duration <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      Duration,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Duration <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Duration, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Duration, 
                germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Amount <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      Amount,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Amount <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Amount, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Amount, 
                germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      InstallmentRatePercentage,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage, 
                germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Age <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      Age,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Age <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Age, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Age, 
                germanTest$Class)


knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_EmploymentDuration <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      EmploymentDuration.lt.1 +
      EmploymentDuration.1.to.4 +
      EmploymentDuration.4.to.7 +
      EmploymentDuration.gt.7 +
      EmploymentDuration.Unemployed,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_EmploymentDuration <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_EmploymentDuration, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_EmploymentDuration, 
                germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Personal <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      Personal.Male.Divorced.Seperated +
      Personal.Female.NotSingle +
      Personal.Male.Single +
      Personal.Male.Married.Widowed +
      Personal.Female.Single,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Personal <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Personal, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Personal, 
                germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Housing <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      Housing.Rent +
      Housing.Own +
      Housing.ForFree,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Housing <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Housing, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_Housing, 
                germanTest$Class)

#Round 5

knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Duration <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      InstallmentRatePercentage+
      Duration,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Duration <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Duration, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Duration, 
                germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Amount <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      InstallmentRatePercentage+
      Amount,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Amount <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Amount, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Amount, 
                germanTest$Class)

knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Age <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      InstallmentRatePercentage+
      Age,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Age <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Age, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Age, 
                germanTest$Class)


knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_EmploymentDuration <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      InstallmentRatePercentage+
      EmploymentDuration.lt.1 +
      EmploymentDuration.1.to.4 +
      EmploymentDuration.4.to.7 +
      EmploymentDuration.gt.7 +
      EmploymentDuration.Unemployed,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_EmploymentDuration <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_EmploymentDuration, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_EmploymentDuration, 
                germanTest$Class)


knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Personal <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      InstallmentRatePercentage+
      Personal.Male.Divorced.Seperated +
      Personal.Female.NotSingle +
      Personal.Male.Single +
      Personal.Male.Married.Widowed +
      Personal.Female.Single,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Personal <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Personal, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Personal, 
                germanTest$Class)


knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Housing <-
  train(
    Class ~ CreditHistory.NoCredit.AllPaid +
      CreditHistory.ThisBank.AllPaid +
      CreditHistory.PaidDuly +
      CreditHistory.Delay +
      CreditHistory.Critical+
      CheckingAccountStatus.lt.0 +
      CheckingAccountStatus.0.to.200 +
      CheckingAccountStatus.gt.200 +
      CheckingAccountStatus.none+
      SavingsAccountBonds.lt.100 +
      SavingsAccountBonds.100.to.500 +
      SavingsAccountBonds.500.to.1000 +
      SavingsAccountBonds.gt.1000 +
      SavingsAccountBonds.Unknown +
      InstallmentRatePercentage+
      Housing.Rent +
      Housing.Own +
      Housing.ForFree,
    data = germanTrain,
    method = "knn"
  )
germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Housing <-
  predict(knnmodel_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Housing, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory_CheckingAccountStatus_SavingsAccountBonds_InstallmentRatePercentage_Housing, 
                germanTest$Class)