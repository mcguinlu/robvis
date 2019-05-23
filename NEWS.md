# robvis 0.2.0 (May 2019)

* Major updates
  * Removed "save" argument from both the `rob_summary()` and `rob_traffic_light()` functions. In line with CRAN guidelines, this element was removed as packages should not write to the file system. 
  * Added "weighted" argument to `rob_summary()` to allow users to choose whether to create a weighted or unweighted barplot. 
  * Added a new generic template (tool = "ROB1") which allows users to have a nonstandard number of bias domains. This template takes the columns headings of user-defined domains and passes them to the caption of the resulting figure.
  
* Minor updates
  * New functionaility for users to choose color-scheme or provide their own vector of colors for use in the plots. Preset options include traditional "Cochrane" colours and a colourblind friendly pallete. 
  * Improved text processing to allow for imperfect matching of judgements, primarily to allow for differences in cases (e.g. "low" will match with "Low"), minor spelling errors (e.g. "loq" will match with "Low"), and a leading whitespace (e.g. " loq" will match with "Low").
  

# robvis 0.1.0 (April 2019)

* Initial working version of package
