# LinearModelR

This [Shiny](https://shiny.posit.co/) app serves as a wrapper for the R package _limma_, enabling complex, multivariate statistical modeling in an accessible and user-friendly way. If you intend to use this App for data analysis which will later be published, please cite [Brunelli et al., 2026](DOI HERE) AND [Ritchie et al., 2015](10.1093/nar/gkv007).
___________________

## Quick Start

This app can run locally on your desktop computer (Windows/Mac/Linux).

#### Software requirements:
- [The latest version of R](https://cran.r-project.org/)
- [RStudio](https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads)
- The following R-packages. If you do not have them downloaded, copy and paste the following code into your R console:

``` {r}
if (!requireNamespace("BiocManager", quietly = TRUE)) {install.packages("BiocManager")}
```
``` {r}
BiocManager::install(c(“shiny”, “limma”, “cmapR”))
```

The easiest way to access this app is by running the following code in your R console:
``` {r}
shiny::runGitHub("LinearModelR", "melinabrunelli")
```

For more information on how to run and navigate LinearModelR, please refer to our [User Guide](https://github.com/melinabrunelli/LinearModelR/blob/main/LinearModelR_UserGuide_v1.0.pdf).




