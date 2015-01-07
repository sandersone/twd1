##    classTools package for R
##
#' @title Choose The Best Classifier
#'
#' @description
#' 
#' For an object of class \code{formula} and a data set the 
#'
#' \code{chooseClassifier} computes a \link{ggplot} showing
#' the Area Under the Curve (result of \link[ROCR]{performance}) for
#' classification algorithms like \link{naiveBayes}, \link{lda}, 
#' logistic regression \link{glm} with \code{logit} or \code{probit} link function.
#' Any classifier can be added if needed. Contact: 
#' \href{https://github.com/sandersone/twd1/issues}{https://github.com/sandersone/twd1/issues}.
#' 
#' @note
#' 
#' For naive Bayes a parameter \code{laplace} is set to 0.2.
#' For SVM parameters are \code{type='C'} and \code{kernel="radial"}.
#' 
#'
#' @param formula A \code{formula} for classifiers to compute.
#' @param train A \code{data.frame} denoting the training set.
#' @param test A \code{data.frame} denoting the test set.
#' @param choice A \code{vecoter} of length 8 specifying which classifiers 
#' should be compared. The order is as follows: Naive Bayes, LDA, Logit, Probit,
#' SVM, Classification Tree, Boosting, Bagging. The default is \code{c(1,1,1,0,0,0,0,0)}
#' which corresponds to Naive Bayes, LDA and Logit classifiers.
#' 
#'
#'
#' @examples
#' library(foreign)
#' se <- read.arff("http://archive.ics.uci.edu/ml/machine-learning-databases/00266/seismic-bumps.arff")
#' index <- 1:(2*nrow(se)/3)
#' se <- se[,-c(14:16)]
#' se_wyb <- se[,-c(9,15)]
#' train <- se_wyb[ index, ] 
#' test <- se_wyb[ -index, ]
#' chooseClassifier( class~., train, test, rep(1,8) )
#' 
#' 
#' chooseClassifier( class~., train, test, c(1,0,1,0,1,0,1,0) )
#' 
#' @family classTools
#' @rdname chooseClassifier
#' @export
chooseClassifier <- function( formula, train, test, choice=c(1,1,1,0,0,0,0,0)){
   assert_that( is.data.frame( train ), is.data.frame( test ),length(choice)==8 )
   formula <- as.formula( formula )
   
   # begin auc_prec data.frame
   
   auc_prec <- data.frame(AUC=0,
                          PREC=0,
                          CLASSIFIER="Dummy")
   choice <- as.logical(choice)

   # bayes
   if(choice[1]){
   bayes <- naiveBayes( formula , data = train, laplace = 0.2)
   bayes_pred <- predict( bayes, newdata = test )
   bayes_prawd <- predict( bayes, newdata = test, type="raw")[,2]

   auc_prec <- rbind(auc_prec, data.frame(AUC=auc( bayes_prawd, test[, as.character( formula )[2]] ),
                           PREC=sum(diag(table(bayes_pred,test[, as.character( formula )[2]])))/sum((table(bayes_pred,test[, as.character( formula )[2]]))),
                          CLASSIFIER="Naive Bayes"))
   }
   
   # lda
   if(choice[2]){
   mod_lda <- lda( formula, data = train )
      
   pred_klas <- predict( mod_lda, newdata = test )$class
   pred_praw <- predict( mod_lda, newdata = test )$posterior[,2]

   auc_prec <- rbind(auc_prec,data.frame(AUC=auc( pred_praw, test[, as.character( formula )[2]] ),
                                 PREC=sum(diag(table(pred_klas,test[, as.character( formula )[2]])))/sum((table(pred_klas,test[, as.character( formula )[2]]))),
                                 CLASSIFIER="LDA"))
   }
   
   # logit
   if(choice[3]){
   logit  <-  glm( formula, data = train, family=binomial(link = "logit"))
   P <- predict( logit, newdata = test, type="response")
   Pred  <-  ifelse( P >0.5, 1, 0 )

   auc_prec <- rbind(auc_prec,data.frame(AUC= auc( P, test[, as.character( formula )[2]] ),
                  PREC=sum(diag(table(Pred,test[, as.character( formula )[2]])))/sum((table(Pred,test[, as.character( formula )[2]]))),
                  CLASSIFIER="Logit"))
   }
   
   # probit
   if(choice[4]){
   probit  <-  glm( formula, data = train, family=binomial(link = "probit"))
   P <- predict( probit, newdata = test, type="response")
   Pred  <-  ifelse( P >0.5, 1, 0 )

   auc_prec <- rbind(auc_prec,data.frame(AUC= auc( P, test[, as.character( formula )[2]] ),
                                PREC=sum(diag(table(Pred,test[, as.character( formula )[2]])))/sum((table(Pred,test[, as.character( formula )[2]]))),
                                CLASSIFIER="Probit"))
   }
   
   #svm
   if(choice[5]){

   
   SVM <- svm(formula, train,type='C',kernel="radial", 
          probability=TRUE)
    
   pred1 <- predict(SVM, test, probability=TRUE)
   SVM_pred <- attr(pred1,"probabilities")[,2]
   SVM_class <- ifelse( SVM_pred >0.5, 1, 0 )
   

   auc_prec <- rbind(auc_prec,data.frame(AUC= auc( SVM_pred, test[, as.character( formula )[2]] ),
                                      PREC=sum(diag(table(SVM_class,test[, as.character( formula )[2]])))/sum((table(SVM_class,test[, as.character( formula )[2]]))),
                                      CLASSIFIER="SVM"))
   }

   # drzewo 
   if(choice[6]){

   drzewo <- rpart(formula,data=train)
   drzewo_pred <- predict(drzewo,newdata=test)[,2]
   P <- ifelse(drzewo_pred>0.5,1,0)

   auc_prec <- rbind(auc_prec,data.frame(AUC= auc( drzewo_pred, test[, as.character( formula )[2]] ),
                                      PREC=sum(diag(table(P,test[, as.character( formula )[2]])))/sum((table(P,test[, as.character( formula )[2]]))),
                                      CLASSIFIER="Tree"))

   }
   
   # boosting 
   
   if(choice[7]){
   boost <- boosting(formula, data = train)
   boost_pred <- predict(boost,test)$prob[,2]
   P <- ifelse(boost_pred>0.5,1,0)

   auc_prec <- rbind(auc_prec,data.frame(AUC= auc(boost_pred, test[, as.character( formula )[2]] ),
                                      PREC=sum(diag(table(P,test[, as.character( formula )[2]])))/sum((table(P,test[, as.character( formula )[2]]))),
                                      CLASSIFIER="Boost"))
   }
      
   # bagging
   
   if(choice[8]){
   bag <- bagging(formula, data = train)
   bag_pred <- predict(bag,test)$prob[,2]
   P <- ifelse(bag_pred>0.5,1,0)
   
   auc_prec <- rbind(auc_prec,data.frame(AUC= auc(bag_pred, test[, as.character( formula )[2]] ),
                                         PREC=sum(diag(table(P,test[, as.character( formula )[2]])))/sum((table(P,test[, as.character( formula )[2]]))),
                                         CLASSIFIER="Bag"))

   }
   auc_prec <- auc_prec[-1,]
   x <- table(test[, as.character( formula )[2]])
   that <- which(x == max(x))
   
   plotClassifiers(data.frame(auc_prec), line = x[that]/sum(x) )
}



auc <- function(predicted_probability, real_classes){
   pred <- prediction(predicted_probability, real_classes)
   performance(pred, "auc")@y.values[[1]] 
}


plotClassifiers <- function( aucesX, line ){
 # auc_prec -> aucesX
   names(aucesX) <- c("auc", "prec","algorithm")
   
   ggplot(aucesX, aes(x= algorithm, y=auc, fill= auc))+ 
      geom_bar(stat = "identity")+
      geom_point(aes(x=algorithm,y=prec))+
      geom_text(aes(x=algorithm,y=prec,label=paste0(round(prec,3)*100,"%"),size=prec),vjust=1.8)+
      scale_size(range=c(4,7))+
      scale_fill_gradient(low="plum1", high="orchid4")+
      xlab("Algorithms")+
      ylab("Area Under the Curve / Precision")+
   
      theme( axis.text.x = element_text(family = "mono", size=15),
          axis.title.x= element_text(family = "mono", size=15),
          axis.title.y= element_text(family = "mono", size=15),
          panel.grid.major.x = element_blank(), 
          title =element_text(family = "mono", size = 18)#, legend.position = "top"
          ) +ggtitle("Comparison of chosen \n classification algorithms")+
      geom_hline(yintercept=line)+
      annotate("text", x = 0.6, y =line-0.02 , label = "proportion of the \n most numerous \n class", size=3)
   

}
