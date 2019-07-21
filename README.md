# `rabm`



<p align="center">
  <img src="https://raw.githubusercontent.com/davidclarance/rabm/master/rabmhex.png" alt="drawing" width="200"/>
</p>



The `rabm` package provides an interface in R to the Africa Bird Atlas API. 


The Africa Bird Atlas is based out of the University Of Cape Town and is funded by the [FitzPatrick Institute of African Ornithology](http://www.fitzpatrick.uct.ac.za/) and the [South African National Biodiversity Institute](https://www.sanbi.org/). The project is actively supported by[ BirdLife South Africa](http://birdlife.org.za/) and [BirdLasser](https://www.birdlasser.com/).



## Installing the package


The package is in development and therefore isn't hosted on CRAN. Therefore, the regular `install.packages()` technique will not work. To install the packages please follow the steps outlined below. 


Step 1: Install `devtools`

```r

install.packages("devtools")

```


Step 2: Install `rabm` by running the following code:

```r

devtools::install_github("davidclarance/rabm")

```

You only need to do Step 1 and Step 2 once on your computer. 


Step 3: Load the library

```r

library(rabm)

```

You'll need to do Step 3 every time you use the library. 


## Using the package

You can get cracking on the package right away. 


Please read through the [extracting data vignette here](https://davidclarance.github.io/rabm/articles/rabm-vignette.html) to learn how to extract data. 



## Getting help


If you're stuck please go through the following steps:


1. Use the `?`: In your R terminal, you can do `?extract_all` to get help with the `extract_all` function. 

2. Read the documentation [here](https://davidclarance.github.io/rabm/reference/index.html). 

3. Read through the case studies [here](https://davidclarance.github.io/rabm/articles/rabm-vignette.html)

4. Contact the package developer [David Clarance](mailto:dclarance@gmail.com)

