context("DGEtools - tests for topTable.merge.R functions")


test_that("topTable.merge.R: topTable.merge()", {
    skip_if(suppressWarnings(is.null(getType(DGEobj1, "topTable"))))

    # creating toptables list
    ttList         <- getType(DGEobj1, "topTable")
    contrast_table <- topTable.merge(ttlist = ttList, digits = 2)
    expect_setequal(object = colnames(contrast_table),
                    expected = apply(X        = expand.grid(c("logFC", "AveExpr", "P.Value", "adj.P.Val"), names(ttList)),
                                     MARGIN   =  1,
                                     FUN      =  paste,
                                     collapse = "_"))

    # testing assert statements
    expect_error(topTable.merge(ttlist = NULL),
                 regexp = "ttlist must be specified, be of class 'list' and be a named list specifically, and include items of class 'data.frame'.")
    expect_error(topTable.merge(ttlist = ttList, digits = 1:5),
                 regexp = "digits must be either of length 1 or the same length as colNames.")
})
