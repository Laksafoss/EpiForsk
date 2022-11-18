# EpiForsk

This package is a framework for sharing guides, examples, and functions here at 
EpiForsk. It is primarily managed by ADLS and KIJA, but is intended to be a 
collaborative effort and we encourage sharing your hard earned code snippets. 

## Installation & Use
To install the package write
```{r}
install.packages("EpiForsk")
```
And to load the package write
```{r}
library("EpiForsk")
```

## Availability
Only on github so far. We hope to upload to CRAN soon :) 

## What is already in the package
The package is (hopefully) constantly under development, and to see all content
currently avalible in the package use 

```{r}
help(package = "EpiForsk")
```

## What is suited for the package
There is a strict ban on any and all individual level information! Your code and
examples should strive to be as general as possible to avoid project and person 
specific information. 

For the code to be usable by your colleagues it should strive to be stripped of 
project specific details, allowing for generally transferable ideas to shine 
through.

There are two main formats for contributing: 

### Vignettes
Vignettes are a loose format guide with both description text and examples. 
In vignettes we share examples of typical data management, analysis methods,
and other blog/article style walkthroughs.  

### Functions
Functions automate common tasks the frequently occure in our daily work. These 
will work as any other functions made avalible by other packages in R. However,
the goal is not to make the sleakest, fastest and most efficient versions of
these functionalities, but rather implement functionalities tailored to our 
specific needs. 


## How to contribute
We encourage you to write your contribution yourself. To get started, read the
"contributing" vignette. If this is out of scope, you are welcome to contact 
ADLS or KIJA and we talk about possible solutions.   

## Requirements for contributions
The package MUST be self-sufficient. This means that any data you wish to use 
in your examples should either be simulated in the example or made available as 
a dummy data set withing the package. 

In general we follow [Hadley's guide](https://r-pkgs.org/){target="_blank"} for package writing,
and this book contains a plethora of good advice. 


### Functions
As the package must complie with the CRAN check rules all functions must have a 
documentation. Moreover, we require that this documentation is made via 
[Roxygen2](https://roxygen2.r-lib.org/index.html){target="_blank"} and contains one or more 
examples. 

### Vignettes
So far we have no formal requirements for vignettes.
