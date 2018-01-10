## ----Knitr_Global_Options, include=FALSE---------------------------------
library("knitr")
opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE, autodep = TRUE, tidy = FALSE)

## ----Read_Test-----------------------------------------------------------
library("tidyverse")
CreditTraining <- read_csv("CreditTraining.csv",
                           locale = locale(decimal_mark = ","))
glimpse(CreditTraining)

## ------------------------------------------------------------------------
CreditTesting <- read_csv2("CreditTesting.csv")
glimpse(CreditTesting)

## ----Fix_Type------------------------------------------------------------
Fix_Type <- function(Credit) {
Credit <- dplyr::mutate(Credit, Id_Customer = factor(Id_Customer),
                        Y = factor(Y),
                        BirthDate = lubridate::dmy(BirthDate),
                        Customer_Open_Date = lubridate::dmy(Customer_Open_Date),
                         Prod_Decision_Date = lubridate::dmy(Prod_Decision_Date),
                        Prod_Closed_Date = lubridate::dmy(Prod_Closed_Date))
}
CreditTraining <- Fix_Type(CreditTraining)
CreditTesting <- Fix_Type(CreditTesting)

## ----Fix_Levels----------------------------------------------------------
summary(CreditTraining)
Fix_Levels <- function(Credit) {
  Credit <- dplyr::mutate(Credit, Y = factor(Y, levels = c(1,0), 
                                      labels = c("DEFAULT","NO_DEFAULT")),
                          Customer_Type = factor(Customer_Type, 
                                                  levels = c("Non Existing Client", "Existing Client")),
                          Educational_Level = factor(Educational_Level, 
                                            levels = c("Secondary or Less",
                                                       "Diploma",
                                                       "University",
                                                       "Master/PhD")))
}
CreditTraining <- Fix_Levels(CreditTraining)
CreditTesting <- Fix_Levels(CreditTesting)

## ----Variable_Inspection-------------------------------------------------
nameCreditTraining <- names(CreditTraining)
for (varname in nameCreditTraining[-1]) {
  print(summary(dplyr::select(CreditTraining, one_of(varname))))
  NbNa <-sum(is.na(CreditTraining[[varname]])); 
  if (NbNa >0) {
    writeLines(strwrap(paste("\n",varname,"has",NbNa, "NA")))
  }
  print(qplot(data = CreditTraining, get(varname), xlab = varname))
}

## ----Fix_NA--------------------------------------------------------------
ComputeMedOrMod <- function(x) {
  if (is.factor(x)) {
    y=levels(x)[which.max(table(x))]
  }
  else
  {
    y=median(x,na.rm=TRUE)
  }
  return(y)
}

NAvalue=list()
for (name in names(CreditTraining)) {
  NAvalue[[name]]=ComputeMedOrMod(CreditTraining[[name]])
}
NAvalue[["Prod_Closed_Date"]] <- NULL

Fix_NA <- function(Credit, NAvalue. = NAvalue) {
  for (name  in names(NAvalue.)) {
    Credit[[name]][is.na(Credit[[name]])]=unlist(NAvalue.[[name]])
  }
  Credit
}
CreditTraining <- Fix_NA(CreditTraining)
CreditTesting <- Fix_NA(CreditTesting)

## ----Fix_Prod_Closed_Date------------------------------------------------
Fix_Prod_Closed_Date <- function(Credit) {
  Credit <- dplyr::mutate(Credit, Prod_Closed_Date_NA = is.na(Prod_Closed_Date))
  Credit[["Prod_Closed_Date"]][Credit$Prod_Closed_Date_NA] = max(Credit[["Prod_Closed_Date"]], na.rm = TRUE)
  Credit
}
CreditTraining <- Fix_Prod_Closed_Date(CreditTraining)
CreditTesting <- Fix_Prod_Closed_Date(CreditTesting)

## ----Ploy_Y_Vs-----------------------------------------------------------
library("scales")
nameCreditTraining <- names(CreditTraining)
for (varname in nameCreditTraining[-c(1,2)]) {
  p <- ggplot(data = CreditTraining, aes(x = get(varname), fill = Y)) + xlab(varname)
  switch(class(CreditTraining[[varname]]),
  logical = {p <- p + geom_bar(position = "fill")},
  numeric = {p <- p + geom_histogram(position = "fill")},
  Date = {p <- p + geom_histogram(position = "fill")},
  factor = {p <- p + geom_bar(position = "fill")},
  character = {p <- p + geom_bar(position = "fill")})
  
  print (p + scale_y_continuous(label = percent_format())    +  theme(axis.text.x =
            element_text(size  = 10,
                         angle = 45,
                         hjust = 1,
                         vjust = 1)) )
}

## ----RemoveId------------------------------------------------------------
CreditTraining <- dplyr::select(CreditTraining, -Id_Customer)
CreditTesting <- dplyr::select(CreditTesting, -Id_Customer)

## ----Caret_Basic---------------------------------------------------------
library(caret)
CreditGlm <- train(Y ~ ., data = CreditTraining, method = "glm", metric = "Accuracy")
CreditGlm

## ----Caret_Basic2--------------------------------------------------------
PredGlm <- predict(CreditGlm, newdata = CreditTraining)
head(PredGlm)
PredGlm_test <- predict(CreditGlm, newdata = CreditTesting)
ProbGlm <- predict(CreditGlm, newdata = CreditTraining, type = "prob")
head(ProbGlm)

## ----Caret_Control-------------------------------------------------------
trControlCV <- trainControl(method = "CV",
                          number = 5)
CreditGlm <- train(Y ~ ., data = CreditTraining, method = "glm",
                   trControl = trControlCV)
CreditGlm

PredGlm <- predict(CreditGlm, newdata = CreditTraining)
head(PredGlm)

ProbGlm <- predict(CreditGlm, newdata = CreditTraining, type = "prob")
head(ProbGlm)

## ----Caret_Glm_Results---------------------------------------------------
ErrsCaret <- function(model, name) {
  Errs <- data.frame(model$resample)
  dplyr::mutate(Errs, model = name)
} 

ErrCaretAccuracy <- function(Errs) {
  Errs <- group_by(Errs, model)
  cbind(dplyr::summarize(Errs, mAccuracy = mean(Accuracy, na.rm = TRUE), mKappa = mean(Kappa, na.rm = TRUE),
                             sdAccuracy = sd(Accuracy, na.rm = TRUE), sdKappa = sd(Kappa, na.rm = TRUE)))
}

ErrsGlm <- ErrsCaret(CreditGlm, "Glm")
ErrsGlm

ErrGlm <- ErrCaretAccuracy(ErrsGlm)
ErrGlm

## ----Caret_Parallel------------------------------------------------------
system.time(train(Y ~ ., data = CreditTraining, method = "glm",
                   trControl = trControlCV))
library("doFuture")
registerDoFuture()
plan(multiprocess)
system.time(train(Y ~ ., data = CreditTraining, method = "glm",
                   trControl = trControlCV))

## ----Caret_Glm_ROC-------------------------------------------------------
library(pROC)
GlmROC <- roc(CreditTraining[["Y"]], ProbGlm[[1]])
plot(GlmROC)

auc(GlmROC)

## ----Caret_Glm_ROC_ggplot------------------------------------------------
ToROCDF <- function(ROC, name) {
ROCDF <- data.frame(spec = ROC[["specificities"]], sens = ROC[["sensitivities"]], model = name)
}

GlmROCDF <- ToROCDF(GlmROC, "Glm")

PlotROCDF <- function(ROCDF) {
  ggplot(data = ROCDF, aes(x = spec, y = sens, color = model)) + geom_line() + scale_x_reverse() +
    geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), linetype = 2, color = "black")

}
PlotROCDF(GlmROCDF)

## ----Caret_GLM_Glmnet----------------------------------------------------
CreditGlmnet <- train(Y ~ ., data = CreditTraining, method = "glmnet",
                   trControl = trControlCV)
CreditGlmnet

ggplot(CreditGlmnet)

## ----Caret_GLM_Glmnet_Grid-----------------------------------------------
CreditGlmnet <- train(Y ~ ., data = CreditTraining, method = "glmnet",
                   trControl = trControlCV,
                   tuneGrid  =  expand.grid(alpha = exp(seq(-8,0, length.out = 10)), 
                                          lambda = exp(seq(-8,0, length.out = 10))))
CreditGlmnet

ggplot(CreditGlmnet)

## ----Errs_Glm------------------------------------------------------------
summary(resamples(list(Glm = CreditGlm, Glmnet = CreditGlmnet)))

Errs <- ErrsCaret(CreditGlm, "Glm")
Errs <- rbind(Errs, ErrsCaret(CreditGlmnet, "Glmnet"))
Errs

Err <- ErrCaretAccuracy(Errs)
Err

## ----Caret_Glm_ROC_Crit--------------------------------------------------
trControlCVROC <- trainControl(method = "CV",
                          number = 5,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)
CreditGlmROC <- train(Y ~ ., data = CreditTraining, method = "glm",
                   trControl = trControlCVROC)
CreditGlmROC

CreditGlmnetROC <- train(Y ~ ., data = CreditTraining, method = "glmnet",
                   trControl = trControlCVROC,
                   tuneGrid  =  expand.grid(alpha = exp(seq(-8,0, length.out = 10)), 
                                          lambda = exp(seq(-8,0, length.out = 10))))
CreditGlmnetROC

## ----Credit_NB-----------------------------------------------------------
CreditNB <-  train(Y ~ ., data = CreditTraining, method = "nb",
                    trControl = trControlCV, tuneGrid = expand.grid(fL = 10, usekernel = c(TRUE, FALSE), adjust = TRUE)
                    )
Errs <- rbind(Errs, ErrsCaret(CreditNB, "NB"))

## ----Credit_kNN----------------------------------------------------------
CreditKNN <-  train(Y ~ ., data = CreditTraining, method = "knn",
                   trControl = trControlCV)
Errs <- rbind(Errs, ErrsCaret(CreditKNN, "KNN"))

## ----Credit_SVM----------------------------------------------------------
CreditSVM <- train(Y ~ ., data = CreditTraining, method = "svmLinear",
                   trControl = trControlCV)
Errs <- rbind(Errs, ErrsCaret(CreditSVM, "SVM"))

CreditSVMPoly <- train(Y ~ ., data = CreditTraining, method = "svmPoly",
                   trControl = trControlCV)
Errs <- rbind(Errs, ErrsCaret(CreditSVMPoly, "SVMPoly"))

CreditSVMRadial <- train(Y ~ ., data = CreditTraining, method = "svmRadial",
                   trControl = trControlCV)
Errs <- rbind(Errs, ErrsCaret(CreditSVMPoly, "SVMRadial"))

## ----Credit_NN-----------------------------------------------------------
CreditNN <- train(Y ~ ., data = CreditTraining, method = "nnet",
                   trControl = trControlCV)
Errs <- rbind(Errs, ErrsCaret(CreditNN, "NN"))

## ----Credit_Trees--------------------------------------------------------
CreditBagging <- train(Y ~ ., data = CreditTraining, method = "treebag",
                   trControl = trControlCV)
Errs <- rbind(Errs, ErrsCaret(CreditBagging, "Bagging"))

CreditRF <- train(Y ~ ., data = CreditTraining, method = "rf",
                   trControl = trControlCV)
Errs <- rbind(Errs, ErrsCaret(CreditRF, "RF"))

CreditC5 <- train(Y ~ ., data = CreditTraining, method = "C5.0",
                   trControl = trControlCV)
Errs <- rbind(Errs, ErrsCaret(CreditC5, "C5.0"))

## ----Credit_xgboost, eval = TRUE-----------------------------------------
CreditxgbLinear <- train(Y ~ ., data = CreditTraining, method = "xgbLinear",
                   trControl = trControlCV)
Errs <- rbind(Errs, ErrsCaret(CreditxgbLinear, "Xgboost"))

CreditxgbTree <- train(Y ~ ., data = CreditTraining, method = "xgbTree",
                   trControl = trControlCV)
Errs <- rbind(Errs, ErrsCaret(CreditxgbTree, "Xgboost Tree"))

## ----Credit_Err----------------------------------------------------------
Err <- ErrCaretAccuracy(Errs)
Err

ggplot(data = Err, aes(x = model, y = mAccuracy)) + geom_point(size = 3)

