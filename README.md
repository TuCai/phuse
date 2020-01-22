---
title: "README"
author: "Hanming Tu"
date: "01/21/2020"
output: html_document
---

## About this package

This is my very first R package. The purpose of this package is to provide a framework of PHUSE script web application based on [script metadata for sharing](https://www.phuse.eu/documents//working-groups/deliverables/PHUSE-script-metadata-for-sharing-whitepaper-19810.pdf). See the paper [Defining Script Metadata for Sharing: Using phuse R package as an example](https://www.phusewiki.org/docs/Conference%202017%20CT%20papers/CT12.pdf) and [presenation](https://www.phusewiki.org/docs/Conference%202017%20CT%20Presentations/CT12.pdf) about this framework. Here is a list of [people](https://www.phusewiki.org/wiki/index.php?title=Script_Metadata_for_Sharing) who work on this project.  

It has the following tabs in the framework: 

* **Script**: 
  displays the script if it is readable.
* **YML**: 
  displays the content of YML
* **Info**: 
  displays the information about the YML
* **Metadata**: 
  shows the metadata of the script in table format
* **Verify**: 
  verifies the existence of the files defined in YML
* **Download**: 
  downloads the script to local computer
* **Merge**: 
  merges online and local metadata files
* **Execute**: executes the script if it is executable. 

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
start_app()               # start with default settings
start_app(msg_lvl = 3)    # start and display detailed message at level 3
start_app(n=7)            # to start an application
```

It will try to conduct the following tasks:
* Build a YML file index and stored the file to your local computer if you are the first time to start the interface or the local YML file is too old (older than 7 days)
* Build a data frame to hold the information for all YML files
* Write the data frame to the local file if there are new YML files added
* Populate the “Select Script” dropdown list

## Create Simplified TS domain file

After you install phuse package, you can start it as start_app(n=7) or access the published app at [Creating TS Domain](https://geotiger.shinyapps.io/07_genTS/). 

To view source code, you can go to the [app 07 source](https://github.com/TuCai/phuse/blob/master/inst/examples/07_genTS/app.R). 
