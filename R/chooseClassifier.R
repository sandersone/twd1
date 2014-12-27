##    classTools package for R
##
#' @title Choose The Best Classifier
#'
#' @description
#' 
#' For an object of class \code{formula} and a data set the 
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
#' 
#'
#' @param formula A \code{formula} for classifiers to compute.
#' @param train A \code{data.frame} denoting the training set.
#' @param test A \code{data.frame} denoting the test set.
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
#' chooseClassifier( class~., train, test )
#' 
#' @family classTools
#' @rdname chooseClassifier
#' @export
chooseClassifier <- function( formula, train, test ){
   assert_that( is.data.frame( train ), is.data.frame( test ) )
   formula <- as.formula( formula )
   auces <- list() 
   
#    choosing best k for knn
#    knn_auc <- sapply( 2:6, function( x ){
#       auc( attr( knn( cl = train[, as.character( formula )[2]], 
#                       train = train, test = test, k = x ), "prob" ),
#            test[, as.character( formula )[2]] )
#    })
#    auces$k_knn_auc <- which( knn_auc == max( knn_auc ) )
   
   # bayes
   bayes <- naiveBayes( formula , data = train, laplace = 0.2)
   bayes_pred <- predict( bayes, newdata = test )
   bayes_prawd <- predict( bayes, newdata = test, type="raw")[,2]
   
   auces$bayes_auc <- auc( bayes_prawd, test[, as.character( formula )[2]] )
   
   # lda
   mod_lda <- lda( formula, data = train )
      
   pred_klas <- predict( mod_lda, newdata = test )$class
   pred_praw <- predict( mod_lda, newdata = test )$posterior[,2]
   
   auces$lda_auc <- auc( pred_praw, test[, as.character( formula )[2]] )
   
   # logit
   logit  <-  glm( formula, data = train, family=binomial(link = "logit"))
   P <- predict( logit, newdata = test, type="response")
   Pred  <-  ifelse( P >0.5, 1, 0 )
   auces$logit_auc <- auc( Pred, test[, as.character( formula )[2]] )


   # probit
   probit  <-  glm( formula, data = train, family=binomial(link = "probit"))
   P <- predict( probit, newdata = test, type="response")
   Pred  <-  ifelse( P >0.5, 1, 0 )
   auces$probit_auc <- auc( Pred, test[, as.character( formula )[2]] )
   
   
   plotClassifiers( as.list(auces) )

}



auc <- function(predicted_probability, real_classes){
   pred <- prediction(predicted_probability, real_classes)
   performance(pred, "auc")@y.values[[1]] 
}


plotClassifiers <- function( list ){
   assert_that( is.list( list ) )
   n <- length(list)
   
   aucesX <- as.data.frame(t(as.data.frame(list)))
   aucesX[, 2] <- rownames(aucesX)
#    auces[, 3] <- rep(1, n)
#    names(auces) <- c("auc", "algorithm", "fake")
   names(aucesX) <- c("auc", "algorithm")
   ggplot(aucesX, aes(x= factor(algorithm), y=auc, fill= auc))+ 
      geom_bar(stat = "identity")+
         scale_fill_gradient(low="#BA55D3", high="#8968CD")+
      xlab("Algorithms")+
      ylab("Area Under the Curve")+
   theme( axis.text.x = element_text(family = "mono", size=15),
          axis.title.x= element_text(family = "mono", size=15),
          axis.title.y= element_text(family = "mono", size=15),
          title =element_text(family = "mono", size = 18)#, legend.position = "top"
   ) +ggtitle("Comparison of the \n classification algorithms")+
   scale_x_discrete(labels=c("naiveBayes","LDA","logistic \n regression","probit \n regression"))
   
   
   
}
