Classifiers
===========
Authors
> Marcin Kosiński m.p.kosinski@gmail.com

> Emma Sanderson emmasanderson.pl@gmail.com


Downloading `classTools` package:
```{Ruby}
if (!require(devtools)) {
    install.packages("devtools")
    require(devtools)
}
install_github("sandersone/twd1")
```


Make sure you have [rtools](http://cran.r-project.org/bin/windows/Rtools/) installed on your computer.

The list of available functions:
```{Ruby}
help(package="classTools")
?chooseClassifier
```

Follow this example:
```{Ruby}
library(foreign)
se <- read.arff("http://archive.ics.uci.edu/ml/machine-learning-databases/00266/seismic-bumps.arff")
index <- 1:(2*nrow(se)/3)
se <- se[,-c(14:16)]
se_wyb <- se[,-c(9,15)]
train <- se_wyb[ index, ]
test <- se_wyb[ -index, ]
chooseClassifier( class~., train, test, rep(1,8) )
```

![Example 1](https://raw.githubusercontent.com/sandersone/twd1/master/Rplot.jpeg)


We can specify which classifier should be calculated and printed.

```{Ruby}
lo <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/brach3-5klas.txt",
                 header=TRUE)
lo[, 7] <- ifelse(lo[, 7] %in% c(1,2),1,0 )
index <- sample(1:nrow(lo), size=1/2*nrow(lo))
train <- lo[index,]
test <- lo[-index,]
lo[, 7] <- as.integer(lo[, 7])

chooseClassifier( LOC~., train, test, c(1,0,1,0,0,0,0,0) )

```

![Example 2](https://raw.githubusercontent.com/sandersone/twd1/master/Rplot01.jpeg)


<h5> A one bad classifier group! </h5>

```{Ruby}
fit <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/fitness.txt",
                  header = TRUE)

fit[, 8] <- sample(0:1, size = nrow(fit), replace=TRUE)
names(fit)[8] <- "group"
index <- sample(1:nrow(fit), size=1/2*nrow(fit))
train <- fit[index,]
test <- fit[-index,]

chooseClassifier( group~., train, test, 
                  sample(0:1, size = 8, replace=TRUE) )


```



![Example 2](https://raw.githubusercontent.com/sandersone/twd1/master/Rplot02.jpeg)
