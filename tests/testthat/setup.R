require(testthat)
require(DGEobj)
require(DGEtools)
require(limma)
require(edgeR)
require(stats)

# APJ_LAD_Heart_18Jan2017 from our shared directory of DGEobj
DGEobj1 <- readRDS('/efs/bms_shared/data/cdb/APJ_LAD_Heart_18Jan2017.RDS')
