# Welcome


Unlock the power of handwriting analysis with handwriter. This tool is
designed to assist forensic examiners by analyzing handwritten
documents. Whether you are a forensic document examiner, legal
professional, academic, or simply curious about how statistics are
applied to handwriting, handwriter provides an automated way to evaluate
handwriting samples.

# Quick Start

## View a Demo

View a demo of handwriter on handwritten documents from the CSAFE
Handwriting Database. Go to
[demo](https://csafe.shinyapps.io/handwriterAppDemo/).

## Use Your Own Handwriting Samples

### Installation

Handwriter requires R, RStudio IDE, and JAGS.

-   Install R and RStudio from
    [POSIT](https://posit.co/download/rstudio-desktop/)
-   Install JAGS from
    [SourceForge](https://sourceforge.net/projects/mcmc-jags/files/)

Install the handwriterApp R package. Open RStudio and navigate to the
console window and type

``` r
options(timeout = 100000)
install.packages("handwriterApp")
install.packages("devtools")
devtools::install_github("CSAFE-ISU/handwriterRF")
```

### Launch the App

Open RStudio, navigate to the console window, and type:

``` r
library(handwriterRF)
library(handwriterApp)
handwriterApp()
```

In the pop-up window, click **Open in Browser**. If you use the app in
the pop-up window instead of in a browser, some links will not work.

Follow the instructions to analyze handwriting samples.
