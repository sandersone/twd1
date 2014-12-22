# przykladowe dane

library(class)
library(ggplot2)
library("scales")
library("Cairo")
library("grid")

A = read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/earthquake.txt",h=T)
attach(A)
head(A)

train <- test <- A[,2:3]
train.cl <- test.cl <- A[,1]
k=1:10

wykres <- function(train,train.cl,test,test.cl,k=1:10){
   # tutaj bêd¹ stopifnoty
   # tutaj wybieramy te k, które bêd¹ pokazane, max 5
   
   if (length(k)>5) {
      knew=numeric(5)
      knew[1]=k[1]
      knew[5]=k[length(k)]
      knew[3]=ceiling(mean(c(knew[1],knew[5])))
      knew[2]=ceiling(mean(c(knew[1],knew[3])))
      knew[4]=ceiling(mean(c(knew[5],knew[3])))
      
   }
   
   k <- unique(knew)
   dane <- NULL
   l.klas <- length(unique(train.cl))
   
    for (i in k){
      my.knn <- knn(train, test, train.cl, k=i)
      tab <- table(test.cl,my.knn)
      prec<- sum(diag(tab))/sum(tab)
      x <- data.frame(table(my.knn))
      names(x) <- c("klasa","licznosc")
      dane.new <- data.frame(x,k=rep(i,l.klas),prec=rep(prec,l.klas))
      dane <- rbind(dane,dane.new)
   }
   
   punkty <- ((1:(l.klas+1))/(l.klas+1))[1:l.klas]
   
   
   dane$pkt <- rep(punkty,e=length(k))
   rysowanie(dane)
}



rysowanie <- function(dane){
   ggplot(dane, aes(x=k, y=klasa))+
      theme_bw() +
      ggtitle("Precyzja klasyfikacji w zale¿noœci od \nwybranego k w metodzie knn")+
      theme(
         axis.title=element_text(size=14,face="bold"),
         plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank(),
         plot.margin=unit(c(0,0,0,0),"mm"),
         axis.ticks.length=unit(0, "mm"),
         axis.ticks.margin=unit(0, "mm"),
         plot.title = element_text(size = 25)
      )+
      geom_tile(aes(fill=prec))+
      scale_fill_gradient(low="#C2E699", high="#006837")+
      geom_point(aes(size=licznosc))
   
}




train=A[,2:3]

m <- wykres(train,A$popn,train,A$popn,k=1:16)


Knn1 = knn(train,train,cl=A$popn,k=3,prob=FALSE)
c=1
   
if (c>=1) {x=2}
# przyk³adowe KNN
   library(class)

train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
realcl <- cl
moj.knn <- knn(train, test, cl, k = 3)
head(train)
head(test)
x <- data.frame(table(moj.knn))
names(x) <- c("klasa","licznosc")
tab <- table(realcl,moj.knn)
prec <- sum(diag(tab))/sum(tab)

kv <- 1:5
kp <- rep(0,5)
result=data.frame(cbind(kv,kp,num=factor(rep(1, each=5))))

for (i in (1:nrow(result))){
   moj.knn <- knn(train, test, cl, k = result[i,1])
   tab <- table(realcl,moj.knn)
   result[i,2] <- sum(diag(tab))/sum(tab)
}
result

library(ggplot2)
x.cell.boundary <- c(0, 4, 6, 8, 10, 14)
example <- data.frame(
   x = 1:5,
   y = factor(rep(1, each=5)),
   z = 1:5
)

ggplot(result, aes(x=kv, y=num, fill=kp))+
   geom_tile()

result["kp"]<-runif(5)



### coœ z innego pliku


library("ggplot2")
library("scales")
library("Cairo")
library("grid")


liczba.klas <- 3
lk <- 4
klasa <- rep(c("A","B","C"),lk)
punkty <- ((1:(liczba.klas+1))/(liczba.klas+1))[1:liczba.klas]
k <- rep(1:lk,e=liczba.klas)
licznosc <- floor(runif(12)*10)+1

dane <- data.frame(klasa,pkt=rep(punkty,lk),k,licznosc,num=rep(1,12),
                   prec=rep(result$kp,e=liczba.klas))
dane

p <- ggplot(dane, aes(x=k, y=klasa))+
   theme_bw() +
   ggtitle("Precyzja klasyfikacji w zale¿noœci od \nwybranego k w metodzie knn")+
   theme(
      axis.title=element_text(size=14,face="bold"),
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank(),
      plot.margin=unit(c(0,0,0,0),"mm"),
      axis.ticks.length=unit(0, "mm"),
      axis.ticks.margin=unit(0, "mm"),
      plot.title = element_text(size = 25)
   )+
   geom_tile(aes(fill=prec))+
   scale_fill_gradient(low="#C2E699", high="#006837")+
   geom_point(aes(size=licznosc))

p

