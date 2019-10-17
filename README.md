---
title: "README"
author: "Hanming Tu"
date: "10/17/2019"
output: html_document
---

## About this package

This is my very first R package. The purpose of this package is to provide a framework of PHUSE script web application based on [script metadata for sharing](https://www.phuse.eu/documents//working-groups/deliverables/PHUSE-script-metadata-for-sharing-whitepaper-19810.pdf). See the paper [Defining Script Metadata for Sharing: Using phuse R package as an example](https://www.phusewiki.org/docs/Conference%202017%20CT%20papers/CT12.pdf) and [presenation] (https://www.phusewiki.org/docs/Conference%202017%20CT%20Presentations/CT12.pdf) about this framework. Here is a list of [people](https://www.phusewiki.org/wiki/index.php?title=Script_Metadata_for_Sharing) who work on this project.  

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

It will try to conduct the following tasks:
* Clone the phuse-scripts repository to your local computer if you are the first time to start the interface or the local repository is old
* Grep all the YML files from the local repository
* Build a data frame to hold the information for all YML files
* Write the data frame to a local file
* Populate the “Select Script” dropdown list
