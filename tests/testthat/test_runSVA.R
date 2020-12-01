context("DGEtools - tests for runSVA.R functions")


test_that("runSVA.R: runSVA()", {
    dgeObj_sva <- runSVA(dgeObj = DGEobj1, designMatrixName = "RG", saveSv = TRUE)
    expect_s3_class(dgeObj_sva, "DGEobj")
    expect_true(file.exists("svobj.RDS"))

    expect_error(runSVA(designMatrixName = "designMatrix"),
                 regexp = "dgeObj must be specified, be of class 'DGEobj', and should have a 'design' attribute.")
    expect_error(runSVA(dgeObj = DGEobj1),
                 regexp = "designMatrixName must be specified, should be of class 'character', and must exist as an attribute on the dgeObj.")
    expect_error(runSVA(dgeObj = DGEobj1, designMatrixName = "RG", method =  "xyz"),
                 regexp = "method must be one of 'leek' or 'be'.")
    file.remove("svobj.RDS")
})
