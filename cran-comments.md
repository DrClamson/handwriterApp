## Resubmission
This is a resubmission. In this version I have:

* Changed the title from "Shiny App for Handwriting Analysis in R" to "A 'shiny' Application for Handwriting Analysis"

* Placed package names in single undirected quotes, and changed directed quotation marks to undirected quotes in the description text.

* Added "\value{No return value, called to launch 'shiny' app}" to the .Rd file for for the exported method handwriterApp().

* Verified that functions do not write by default or in examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). 

## R CMD check results

0 errors | 0 warnings | 1 note
