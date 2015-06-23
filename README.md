# Demonstrating Sampling Distributions for Various Test Statistics

This shiny app is based on the Distribution of Sample Means shiny app written by Ian Stevenson [https://ihstevenson.shinyapps.io/sample_means/].  

There are some things that break this app, for instance try typing 
```
rnorm <- 70 
```
at the command prompt before you run the app.  It is best to clear the workspace before running the app.  You can clear the workspace and run the app locally using this code:

```
rm(list=ls()) )
require(shiny)
runGitHub("samplingTestStat","DataScienceUWL")
```

If you can make help make the app more robust, then please share!