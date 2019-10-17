---
title: "README"
author: "Hanming Tu"
date: "10/17/2019"
output: html_document
---

## About this package

This is my very first R package. The purpose of this package is to provide a framework of PHUSE script web application based on [script metadata for sharing](https://www.phuse.eu/documents//working-groups/deliverables/PHUSE-script-metadata-for-sharing-whitepaper-19810.pdf). 

It has the following tabs in the framework: 

* Script: 
  displays the script if it is readable.
* YML: 
  displays the content of YML
* Info: 
  displays the information about the YML
* Metadata: 
  shows the metadata of the script in table format
* Verify: 
  verifies the existence of the files defined in YML
* Download: 
  downloads the script to local computer
* Merge: 
  merges online and local metadata files
* Execute: executes the script if it is executable. 

## How to install 


* Install from CRAN

```
install.packages(“phuse”)
```

* Install from GitHub

```
install.packages("devtools")
library(devtools)
install_github(”TuCai/phuse")
```

## How to use

```
library(phuse)
start_app()
start_app(n=7)
```



## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
