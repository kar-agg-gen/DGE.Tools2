context("DGEtools - tests for plotNorm.R functions")


test_that("plotNorm.R: plotNorm()", {

    # testing with DGEobj and plotType - box
    expect_warning({norm_plot <- plotNorm(DGEobj1$counts, plotType = "box")})
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with count matrix and plotType - density
    expect_warning({norm_plot <- plotNorm(DGEobj1, plotType = "density")})
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing assert statements
    expect_error(plotNorm(NULL),
                 regexp = "dat must be of either class 'matrix' or 'DGEobj'.")
    expect_error(plotNorm(DGEobj1, plotType = "heatmap"),
                 regexp = "plotType must be one of 'box' or 'density'.")
    expect_error(plotNorm(DGEobj1, normalize = "xyz"),
                 regexp = "normalize must be one of 'TMM', 'RLE', 'upperquartile', or 'none'.")
})
