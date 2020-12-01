context("DGEtools - tests for tidyIntensity.R functions")


test_that('tidyIntensity: tidyIntensity()', {

    groups <- paste("group", factor(rep(1:4, each = 100)), sep = "")
    tidyIntensity_one_input <- matrix(rnorm(2400, mean = 10), ncol = length(groups))
    colnames(tidyIntensity_one_input) <- paste("sample", 1:ncol(tidyIntensity_one_input), sep = "")
    rownames(tidyIntensity_one_input) <- paste("gene", 1:nrow(tidyIntensity_one_input), sep = "")

    tidyIntensity_one_test <- tidyIntensity(tidyIntensity_one_input,
                                            rowIDcolname = "GeneID",
                                            keyColname = "Sample",
                                            valueColname = "Log2CPM",
                                            group = groups)

    expect_true(is.data.frame(tidyIntensity_one_test))
    expect_equal(nrow(tidyIntensity_one_test), 2400)
    expect_equal(ncol(tidyIntensity_one_test), 4)
    expect_equal(names(tidyIntensity_one_test), c("GeneID", "Sample", "Log2CPM", "group"))

    expect_error(tidyIntensity(tidyIntensity_two_test),
                 regexp = "object 'tidyIntensity_two_test' not found")
})


test_that('tidyIntensity: incorrect usage', {
    expect_error(tidyIntensity(),
                 regexp = "argument \"intensityObj\" is missing, with no default")
})
