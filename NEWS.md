# robvis 0.3.0 (August 2019)

* Major updates
  * `rob_traffic_light()` can now handle a summary table with or without a "Weight column". `rob_summary()` still requires a "Weight" column, as it is strongly encouraged that weights be used to create the summary barplot. 
  * Functions can now handle summary tables with less than the expected number of judgments. For example, for ROB2, a sheet with only "Low" and "Some concerns" would cause the resulting graph to be inverted and mess up the ordering of the legend. This issue is now fixed for both `rob_summary()` and `rob_traffic_light()`.
  * The shapes indicating the risk-of-bias level in `rob_traffic_light()` have been updated.
  * `rob_traffic_light()` no longer orders studies alphabetically, instead retaining the order from the uploaded CSV.
  * The order of the domains in `rob_summary()` has been corrected to accurately reflect the order of domains in each tool, rather than presenting them in alphabetical order.
  * Legends for `rob_traffic_light()` are now ordered by ascending/descending (depending on the tool) risk of bias. 
   
* Minor updates
  * `CODE_OF_CONDUCT` and `CONTRIBUTING` documents have been added to the GitHub repo.
  * A PR template has been added to the GitHub repo.


# robvis 0.2.0 (May 2019)

* Major updates
  * Removed "save" argument from both the `rob_summary()` and `rob_traffic_light()` functions. In line with CRAN guidelines, this element was removed as packages should not write to the file system. 
  * Added "weighted" argument to `rob_summary()` to allow users to choose whether to create a weighted or unweighted barplot. 
  * Added a new generic template (tool = "ROB1") which allows users to have a nonstandard number of bias domains. This template takes the columns headings of user-defined domains and passes them to the caption of the resulting figure.
  
* Minor updates
  * New functionality for users to choose color-scheme or provide their own vector of colors for use in the plots. Preset options include traditional "Cochrane" colours and a colourblind friendly palette. 
  * Improved text processing to allow for imperfect matching of judgments, primarily to allow for differences in cases (e.g. "low" will match with "Low"), minor spelling errors (e.g. "loq" will match with "Low"), and a leading whitespace (e.g. " loq" will match with "Low").
  

# robvis 0.1.0 (April 2019)

* Initial working version of package
