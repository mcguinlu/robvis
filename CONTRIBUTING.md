# Contributing to robvis

The goal of this guide is to help contribute to robvis as quickly as possible.

Please note that robvis is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By  contributing to this project, you agree to abide by its terms.


### Fixing typos
Small typos or grammatical errors in documentation may be edited directly using the GitHub web interface of your forked repository, so long as the changes are made in the _source_ file.

*  DO: edit a roxygen comment in a `.R` file below `R/`.
*  DO NOT: edit an `.Rd` file below `man/`.

Similarly, typos/errors in the supporting documents (e.g. these contributing guidelines, NEWS.md, CODE_OF_CONDUCT.md) may be edited directly using the GitHub web interface of your forked repository.

The exception to the above is when typos/errors occur in the README.md document. To correct these:

  * Fork the respository to your personal GitHub account
  * Edit the README.Rmd document to fix the error
  * Knit the README.Rmd document to produce the corrected README.md
  * Commit changes and push both corrected documents to your forked respository
  * Issue a pull request (see the section on [Pull Requests](#pull-requests), below).
  

### Filing an issue

When filing an issue, the most important thing is to include a minimal 
reproducible example so that we can quickly verify the problem, and then figure 
out how to fix it. See "[Writing a good reproducible example](https://reprex.tidyverse.org/articles/reprex-dos-and-donts.html)".


1.  **Any additional required packages** should be loaded at the top of the script, so it's easy to
    see which ones the example needs.
  
2.  Spend a little bit of time ensuring that your **code** is easy for others to
    read:
  
    * make sure you've used spaces and your variable names are concise, but
      informative
  
    * use comments generously to indicate where your problem lies
  
    * do your best to remove everything that is not related to the problem.  

You can check you have actually made a reproducible example by starting up a 
fresh R session and pasting your script in.


### Pull Requests

#### Getting Started
* Make sure you have a [GitHub account](https://github.com/signup/free).
* Familiarise yourself with Git and Github, using the [resources](#additional-resources) at the end of this page.

#### Prerequisites
Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem. If you’ve found a
bug, create an associated issue and illustrate the bug with a minimal 
[reproducible example](https://reprex.tidyverse.org/articles/reprex-dos-and-donts.html).

#### Pull request process
*  We recommend that you create a Git branch for each pull request (PR).  
*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html), 
for documentation.  
*  For user-facing changes, add a bullet to the top of [NEWS.md](NEWS.md) below the current
development version header describing the changes made followed by your GitHub
username, and links to relevant issue(s)/PR(s).


### Additional Resources

  * The [Git and Github](http://r-pkgs.had.co.nz/git.html) section of the __R Packages__ book by Hadley Wickham
  * [Happy Git and GitHub for the useR](https://happygitwithr.com/)
  * General GitHub [documentation](https://help.github.com/)
  * GitHub pull request [documentation]](https://help.github.com/articles/creating-a-pull-request/)







