# DGEtools: RNA-Seq Analysis Workflow Package

DGEtools is a suite of functions to facilitate and standardize RNA-Seq DGE analysis.  DGEtools relies on the DGEobj data structure to store DGE data and analysis results.  

## DGE workflow:

DGEtools has modular functions to conduct DGE analysis from counts to contrasts with facility to select detected genes, normalize data (edgeR TMM), linear modeling (limma, voom, and lmFit), and contrast analysis (topTable, topTreat). This process is broken logically into steps so that it is easy to, for example, substitute in a new or customized normalization step and still be able to take advantage of the other pieces of the pipeline. The DGEtools workflow includes support for qualityWeights, duplicateCorrelation, and SVA analysis.  The most important reason  to use DGEtools is that it produces a standardized data object, the DGEobj, that captures and annotates your workstream, making your data better documented and easier to incorporate into downstream integrative analyses.

## Standard data structures for DGE:

DGEtools uses an S3 data class called DGEobj to capture results in a customizable and reusable data object. See the [DGEobj package](**LINK**) for more details.

## QC Plots:

Several QC plots are available to monitor the quality of your results. These include:

**edgeR dispersion plot**   
**voom Mean-Variance plot**   
**plotPvalHist**: Faceted plot of p-value distributions for each contrast to evaluate quality of your Fit.   
**cdfPlot**: Faceted plot of p-value distributions for each contrast to evaluate quality of your Fit. 
**QCplots**: Plot alignment metrics from Omicsoft  

## Data Exploration Plots:

**profilePlot**: Plot LogIntensity vs. LogRatio from topTable dataframes with highlighting of significantly regulated genes.  
**volcanoPlot**: Plot LogRatio vs. NegLogPvalue from topTable dataframes with highlighting of significantly regulated genes.  
**comparePlot**: Compare LogRatios for two samples showing common or uniquely regulated genes  
**ggplotMDS**: Run Multi-Dimensional Scaling analysis and plot the results  


## Other Documentation

* vignettes/DGE.Tools2_Workflow.pdf:  Workflow example  
* vignettes/DGE.ToolsPlotGallery.pdf: code examples for data exploration plots  


## Installation 

It is best to run the install from a fresh R session before loading any packages because loaded packages cannot be updated.

Install or update DGEobj:  

```
    install.packages("DGEtools")
```


## Installation from GitHub

You can also install directly from GitHub. 

```r
    #if you don't have the devtools package already
    install.packages("devtools") 

    devtools::install_git("LINK") 
```   
