### miniMeta - Web application to run meta-analyses

miniMeta is a [shiny](https://shiny.posit.co/) web application 
that makes it easy to run meta-analyses, without having _any_ familiarity 
with R. It is essentially a graphical front-end for R package 
[meta](https://CRAN.R-project.org/package=meta) by Guido Schwarzer. 
It can be run locally within an R installation, or fully online 
from a shiny server.

miniMeta can run meta-analyses of either Randomized Controlled Trials or 
observational studies, in a "live" web interface where the analysis is
automatically updated as data are entered or options are adjusted. It can save 
(export) these analyses as files (RDS files) which can be later loaded (imported)
to miniMeta, or also further processed in R. See the associated documentation
for more information.

**Local installation**

First you need an [R installation](https://cran.r-project.org/). 
Open R and install package [devtools](https://CRAN.R-project.org/package=devtools) 
if you don't already have it:

      install.packages("devtools")

Then to install FluMoDL, open R and give:

      devtools::install_github("thlytras/miniMeta")

This will also bring in all the required dependencies if not already installed, 
including packages [shiny](https://CRAN.R-project.org/package=shiny), 
[meta](https://CRAN.R-project.org/package=meta) and 
[readxl](https://CRAN.R-project.org/package=readxl).

Check back here often for new updates of the package!

**Usage**

Just open R, load the miniMeta package and launch:

      library(miniMeta)
      miniMeta()

A browser window will open, and you can use miniMeta.

If you want to have a quick look at how it works, try running it on [shinyapps.io](https://thlytras.shinyapps.io/minimeta/).
