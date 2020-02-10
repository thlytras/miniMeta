**Basic usage**

All data and analysis options are inputted on the left side of the page, 
and all output are produced on the right side. Options that directly 
affect the output are also found on the right side.

Use the "RCT module" to do a meta-analysis of Randomized Controlled Trials, 
or the "Observational studies module" to do a meta-analysis of observational studies.
Study data are entered manually in the table, or from an Excel file (whose
columns should match those on the table).
Click on the "Show analysis options" switch to adjust all options used for the
meta-analysis, such as type of effect measure (Relative Risk, Odds Ratio, etc),
type of model (fixed-effects or random-effects), etc.

With every change on the left-side input data (study data or analysis options),
the meta-analysis is automatically re-run and the forest plot updated. 
Click "Download plot" to download it as a file.
The "Plot options" tab contains all the options that affect the plot including 
download options; use that to select the type of image downloaded, 
its dimensions, etc. 
The options also affect the funnel plots in the "Funnel plot" tab.

**Import/export**

The entire meta-analysis can be exported as an RDS file, containing a 
serialized R object of class 'miniMeta'. 
This contains the data, analysis options and plot options. 
The file can be imported back into miniMeta, or further processed in the R console. 
In addition, the meta-analysis can be exported as "source code", 
i.e. as a script that can be source'd directly in the R console, in order to allow
finer processing than what is allowed by the miniMeta interface.

All settings (all analysis options and plot options, in both the RCT 
and observational studies module) can be exported into an RDS file 
(containing a list of all settings) and imported back into another miniMeta instance.
Also the settings can be stored in a browser cookie, so that they are 
automatically loaded back the next time miniMeta is used.
Click the "Settings" dropdown menu on the top right corner to access these functions.

**Tools**

There are two small tools included under the "Tools" tab: 
one that implements the Bucher method for adjusted indirect comparisons,
and an Optimal Information Size (OIS) calculator (essentially a sample size 
calculator, see the 
[GRADE handbook](https://gdt.gradepro.org/app/handbook/handbook.html#h.ygojbnr1bi5y) 
for details). Again, all input data are entered on left side of the page, 
and all output are produced on the right side.

