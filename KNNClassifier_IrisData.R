############################################################ KNN Classifier - Iris Data ##########################################

#Loading Iris Data
data("iris")

#Loading Required Libraries
library(class)
library(TunePareto)
library(caret)
library(ROCR)
library(ggplot2)

#Dividing data into Training and Testing
indexes=sample(1:nrow(iris),size=round(0.7*nrow(iris)))
train=iris[indexes,]
test=iris[-indexes,]

#Subsetting Data For Modeling
X_train=train[,-ncol(iris)] #Training Data - Independent Variables
Y_train=train[,ncol(iris)] #Training Data - Dependent Variable
X_test=test[,-ncol(iris)] #Testing Data - Independent Variables
Y_test=test[,ncol(iris)] #Testing Data - Dependent Variable


##################################  Supervised Learning/Classification - KNN ###########################################

#Function Name: Classifier_KNN
#Function Description: Builds KNN Model (class::knn) with Optimal Parameters (k and l) 
#                     using TunePareto Package(TunePareto::tunePareto), Returns the Test Data with Predictions and 
#                     Save the KNN model Results (i.e. Model Summary, Confusion Matrix,
#                     ROC Curve(Binary Classification) etc.,) in specified working directory
#
#Function Arguments/Parameters: 
#      Mandatory Parameters: 
#           Data Specific Paramters: X_train, Y_train, X_test, Y_test
#           Target/Dependent Variable Specific Paramters: case_Class, TargetVarb_Name
#
#      Optional Parameters: 
#           Working Directory Specific Parameters: work_directory
#           Tuning Specific Paramenters: 
#               Model Specific Parameters: k_params, l_params
#                   k_params: Number of neighbours considered
#                   l_params: Minimum Vote for definite Decision, otherwise doubt. 
#                             (More precisely, less than k-l dissenting votes are allowed, 
#                              even if k is increased by ties.)
#               Cross Validation (Objective Functs: Minimizing Classification Error , Maximizing Sensitivity) Specific Parameters: k_fold,n_times
#                           
#
#Arguments/Parameters Description:
#      Mandatory Parameters: 
#           Data Specific Parameters:
#                        X_train: Training Data - Independent Variables
#                        Y_train: Training Data - Dependent Variable
#                        X_test: Testing Data - Independent Variables
#                        Y_test: Testing Data - Dependent Variable
#      Target/Dependent Variable Specific Parameters:
#                       case_Class: Positive class of Target Variable
#                       TargetVarb_Name: Name of the Target/Dependent Variable
#      Optional Parameters:
#           Working Directory Specific Parameters:
#                   work_directory: Working Directory to Save Random Forest Model Results (Model Object (.rds file), Confusion Matrix (.rds), 
#                                   Variable Importance Plot (.jpg), ROC curve (Applicable for Binary Classification Problems)) Default Value: Present Working Directory
#           Tuning Specific Parameters:
#                   Model Specific Parameters:
#                           k_params: Set of k values in Vector form Default Values: c(5,7,9)
#                           l_params: Set of l values in vector form Default Values: c(1,2,3)
#                   Cross Validation (Objective Functs: Minimizing Classification Error , Maximizing Sensitivity) Specific Parameters: 
#                           k_fold: Number of folds (The original sample is randomly partitioned into k equal sized subsamples/folds) Default Value: 10
#                           n_times: Number of times (i.e. How many times we have to repeact the Cross Validation)  Default Value: 10
#
#Assumptions and Dependencies:
#           Assumptions:
#                       Data Prepared according to KNN Model Requirements (Data Preprocessing, Feature Selection, Checking Assumptions etc.,)
#                       k and l are the only tunable parameters
#           Dependencies:
#                       Libraries: class, TunePareto, caret, ggplot2, ROCR
#
#Additional Information:
#           Reference Links:
#                      class Package Documentation:      https://cran.r-project.org/web/packages/class/class.pdf
#                      TunePareto Package Documentation: https://cran.r-project.org/web/packages/TunePareto/TunePareto.pdf
#                      caret Package Documentation:      https://cran.r-project.org/web/packages/caret/caret.pdf
#                      ggplot2 Package Documentation:    https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
#                      ROCR Package Documentation:       https://cran.r-project.org/web/packages/ROCR/ROCR.pdf

#Classifier_KNN Function Definition                    
Classifier_KNN<-function(X_train,Y_train,X_test,Y_test,k_params=c(5,7,9),l_params=c(1,2,3),k_fold=10,n_times=10,
                         case_Class,work_directory=getwd(),TargetVarb_Name){
  
  setwd(work_directory) #Set Working Directory
  
  cat('\n')     
  cat("########### KNN - Tuning Parameters ###########\n")
  cat('\n')
  
  ######################### Parameter Tuning - KNN ######################################
  tuneModel_knn=tunePareto(data = X_train, labels = Y_train,classifier = tunePareto.knn(), 
                           k=k_params,l=l_params,
                           objectiveFunctions = list(cvError(nfold = k_fold, ntimes = n_times, stratified = TRUE),
                                                     cvSensitivity(nfold = k_fold, ntimes = n_times, stratified = TRUE,caseClass =case_Class)))  
  ######### Optimal Parameter Combinations ################
  cat('\n')     
  cat("########### KNN - Optimal Parameter Combinations ###########\n")
  cat('\n')
  cat("########### Optimal k value ###########\n")
  cat(tuneModel_knn$bestCombinations[[1]]$k)
  cat('\n')    
  cat("########### Optimal l value ###########\n")
  cat(tuneModel_knn$bestCombinations[[1]]$l)
  cat('\n')    
  
  ############################    Model Building - KNN/ Training KNN Model #########################
  #Fitting KNN model with Optimal Parameter Combination
  modelFit_knn=knn(X_train,X_test,cl=Y_train,k=tuneModel_knn$bestCombinations[[1]]$k,
                   l=tuneModel_knn$bestCombinations[[1]]$l)
  
  cat('\n')     #Printing KNN Model Summary to R Console
  cat("########### KNN - Model Summary ###########\n")
  print(modelFit_knn)
  
  #Saving Random Forest Model Object to .rds file 
  saveRDS(modelFit_knn,'KNN_Model.rds')
  
  ##############################   Predictions on Test Data         ##################################
  #Predictions(i.e. Class) for Test data
  Y_test_predict=modelFit_knn
  
  
  ################################### Model Performace/Evaluation on Test Data ################################
  #Confusion Matrix on Test Data
  confMatrix_knn=confusionMatrix(Y_test_predict,Y_test,positive =case_Class )
  
  #Printing Confusion Matrix to R Console
  cat('\n')
  cat("########### KNN - Confusion Matrix on Test Data ###########\n")
  cat('\n')
  print(confMatrix_knn)
  
  #Saving Confusion Matrix to .rds file
  saveRDS(confMatrix_knn,'KNN_ConfusionMatrix.rds')
  
  #ROC Curve - Binary Classification
  if(length(unique(Y_test))==2){ 
    
    # ROC Curve and AUC Value
    pred_knn <- prediction(as.numeric(Y_test_predict),as.numeric(Y_test))
    perf_knn <- performance(pred_knn, measure = "tpr", x.measure = "fpr")
    auc <- performance(pred_knn, measure = "auc")
    auc <- auc@y.values[[1]]
    roc.data <- data.frame(fpr=unlist(perf_knn@x.values),tpr=unlist(perf_knn@y.values),model="knn")
    
    #Plotting ROC Curve
    g<- ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) + geom_ribbon(alpha=0.2) + 
      geom_line( aes(y=tpr)) + ggtitle(paste0("ROC Curve w/ AUC=", auc))+theme_minimal()+
      theme(plot.title = element_text(color="#993333", size=16,hjust = 0.5, face="bold"),
            axis.title.x = element_text(color="#993333", size=14, face="bold.italic"),
            axis.title.y = element_text(color="#993333", size=14, face="bold.italic")
      )
    print(g)
    
    #Saving ROC Curve with AUC Value
    ggsave('KNN - ROC Curve with AUC Value.jpg',last_plot())
  }
  
  #Test Data frame with Prediction Classes and Probabilities 
  testDF_Predictions=cbind(X_test,Y_test,Y_test_predict)
  colnames(testDF_Predictions)[c((ncol(testDF_Predictions)-1),ncol(testDF_Predictions))]=
    c(paste0("Actual_",TargetVarb_Name),paste0("Predicted_",TargetVarb_Name))
  
  #Return the Test Data frame with Prediction Classes and Probabilities
  return(testDF_Predictions)
}
  
#Sample Output on Iris Data (Multi Class Classification)
test_Predictions=Classifier_KNN(X_train,Y_train,X_test,Y_test,case_Class='setosa',TargetVarb_Name = 'Species')
