context("DGEtools tests for mapDGEobj.R functions")


test_that('mapDGEobj.R: mapDGEobj()', {

    map_DGEobj <- mapDGEobj(DGEobj1)
    expect_s3_class(map_DGEobj, "igraph")

    expect_error(mapDGEobj(DGeobj1),
                 regexp = "object 'DGeobj1' not found")
    expect_error(mapDGEobj("XYZ"),
                 regexp = "dgeObj must be of class 'DGEobj'.")
})
