# robvis 0.3.0.900 (Development)

* A new function, `rob_blobbogram()` has been added in response to https://github.com/mcguinlu/robvis/issues/102 by @rdboyes.

* Major updates
  * The default point size for `rob_traffic_light()` has been changed from 20 to 10. This is because, following a review of published `robvis` plots, it seems users are not changing the defaults very often, and in almost all cases, a smaller default looks better. **While existing code will continue to work, the resulting plots will look different unless the `psize` argument was explicitly defined.**
  * A template for the QUIPS tool (prognostic studies) has been added.
  * The generic option for both functions now allow for additional customisation, including control over all text in the resulting figure.
  * A new function, `rob_save()`, has been added, which uses data-driven defaults for figure height and width when saving to a file.
  * New functionality so that `robvis` now supports "No information" as a judgment has been added.
  * New colour scheme for the `colour = "colourblind"`argument has been added.
  * The argument for the generic template has changed from "ROB1" to "Generic". To ensure backward compatibility, the "ROB1" argument is still accepted, but a message is returned to indicate that it may be depreciated in the future.
  * The "Generic" template has been updated, so that it no longer maps "Serious" and "Critical" to the same colour/symbol.
  
* Minor updates
  * A major refactoring of the code base has taken place to allow for future template specific functionality.
  * Improved test coverage.
  * Allowed for US spelling of colourblind
  * `rob_tools()` now returns a message indicating the availability of templates for each function. 

# robvis 0.3.0 (October 2019)

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
