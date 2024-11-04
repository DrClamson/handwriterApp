# handwriterApp 2.0.0

## New features

* Added functionality from 'handwriterRF' to the 'shiny' app. The new functionality allows users to perform forensic handwriting comparison of two scanned handwritten documents. This uses the statistical method described by Madeline Johnson and Danica Ommen (2021) <doi:10.1002/sam.11566>. Similarity measures and a random forest produce a score-based likelihood ratio that quantifies the strength of the evidence in favor of the documents being written by the same writer or different writers.

## Minor improvements and fixes

* Added the option to view a full-screen version of images displayed with `singleImageBodyUI()` and `singleImageServer()`.

* Changed the navigation bar to have tabs Home, Scenarios, Scenario 1, Scenario 2, and More. More is a drop-down menu that gives the options About, Contact, and License.

* Created a new function `plot_writer_profiles()` that plots writer profiles on the same set of axes and labels the lines by the document name. This function is similar to `handwriter::plot_cluster_fill_rates()`, but that function labels the lines by writer ID.

* Changed the color of links to light blue in the sidebar and purple in the body.

* Fixed a bug in the closed-set demo where the document previews were displaying correctly.

* Renamed `currentImageUI()` and `currentImageServer()` as `selectImageUI()` and `selectImageServer()` respectively. This change better accommodates the new `singleImageBodyUI()` and `singleImageServer()` module.

* Replaced the phrase "simulate casework" with "use your own samples" in the closed-set method for better clarity.

# handwriterApp 1.0.1

* Changed the title from "Shiny App for Handwriting Analysis in R" to "A 'shiny' Application for Handwriting Analysis"

* Placed package names in single undirected quotes, and changed directed quotation marks to undirected quotes in the description text.

* Added "\value{No return value, called to launch 'shiny' app}" to the .Rd file for for the exported method handwriterApp().

# handwriterApp 1.0.0

* Initial CRAN submission.
