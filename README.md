Classifiers
===========
Authors
> Marcin Kosiñski m.p.kosinski@gmail.com

> Emma Sanderson


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
chooseClassifier( class~., train, test )
```
