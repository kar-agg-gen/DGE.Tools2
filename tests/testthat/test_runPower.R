context("DGEtools - tests for runPower.R functions")


test_that("runPower.R: runPower()", {
    designMatrix <- model.matrix(~ 0 + ReplicateGroup, getItem(DGEobj1, "design"))
    power_plot <- runPower(counts = DGEobj1$counts, designMatrix = designMatrix)
    expect_type(power_plot, "list")

    expect_s3_class(power_plot$ROC, c("gg", "ggplot"))
    expect_s3_class(power_plot$NvP, c("gg", "ggplot"))
    expect_s3_class(power_plot$PowerData, "data.frame")
})
