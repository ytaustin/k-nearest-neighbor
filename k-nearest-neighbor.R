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

set.seed(1)
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
