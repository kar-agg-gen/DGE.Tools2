context("DGEtools - tests for plotPValHist.R functions")


test_that("plotPValHist.R: plotPvalHist()", {

    # testing plotPvalHist with savePlot and facet = TRUE
    pvalMatrix <- extractCol(getType(DGEobj1, "topTable"), "P.Value")
    pval_plot <- plotPvalHist(pvalMatrix, facetFontSize = 14, savePlot = TRUE)
    expect_s3_class(pval_plot, c("gg","ggplot"))
    expect_true(file.exists("PValueHistFacet.png"))
    file.remove("PValueHistFacet.png")

    # testing plotPvalHist with savePlot and facet = FALSE
    pval_plot <- plotPvalHist(as.matrix(pvalMatrix),
                              facet     = FALSE,
                              savePlot  = TRUE,
                              fileNames = c("BMTL", "BMTH", "BMTM", "ena", "sham"))
    expect_true(all(file.exists(paste0(c("BMTL", "BMTH", "BMTM", "ena", "sham"), ".png"))))
    expect_length(pval_plot, 5)

    expect_error(plotPvalHist(pvalMatrix,
                              facet     = FALSE,
                              fileNames = c("BMTL", "BMTH")),
                 regexp = "Number of fileNames does not match column count!")
    file.remove(paste0(c("BMTL", "BMTH", "BMTM", "ena", "sham"), ".png"))
})
