#' Create tidy contrast list of topTable contrast dfs
#'
#' Takes a DGEobj or contrast list as input and merges them into one tidy dataframe.
#' A contrast list is simply a list of topTable contrast dataframes. For
#' example, DGEobj::getType(myDGEobj, "topTable") would retrieve a list of
#' contrasts from a DGEobj.  The contrast list must be a named list as the
#' contrast names are used during the merge operation.
#'
#' The input may or may not have rownames. If supplied rownameColumn does not exist as a colname in
#' the dataframes, it is created from the rownames. In tidy style, the output will have no rownames.
#'
#' The contrast names will be used as a new column in the tidy output format.
#'
#' @param dgeObj A DGEobj or named list of contrast dataframes. (Required)
#' @param rownameColumn Name of the rowname column. If a column by this
#'   name does not exist, it is created from the rownames property.
#' @param includeColumns A character vector of columns to include in the output.
#'   (Default = colnames of the first contrast)
#'
#' @return A dataframe with merged contrast data.
#'
#' @examples
#' \dontrun{
#'   # Get contrasts directly from a DGEobj
#'   myMergedTidyDF <- tidyContrasts(myDGEobj)
#'
#'   # Assemble a list of contrasts from two DGEobjs; just logFC and conf intervals
#'   myContrasts <- c(getType(DGEobj1, "topTable"), getType(DGEobj2, "topTable"))
#'   myMergedTidyDF <- tidyContrasts(myContrasts, includeColumns = c("logFC", "CI.R", "CI.L"))
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr select bind_rows
#' @importFrom DGEobj getType
#'
#' @export
tidyContrasts <- function(dgeObj,
                          rownameColumn = "rownames",
                          includeColumns) {

    assertthat::assert_that(any(c("DGEobj", "list") %in% class(dgeObj)),
                            msg = "dgeObj must be of class 'DGEobj' or 'list'.")

    if ("DGEobj" %in% class(dgeObj)) {
        dgeObj <- DGEobj::getType(dgeObj, "topTable")
        assertthat::assert_that(!(length(dgeObj) == 0),
                                msg = "No topTable dataframes found in dgeObj. Please specify a dgeObj that contains topTable dataframes.")

    }

    # Make sure list contains only dataframes
    assertthat::assert_that(all(sapply(dgeObj, class) == "data.frame"),
                            msg = "dgeObj must only contain dataframes.")

    # Make sure each df has a name
    minNameLen <- min(sapply(names(dgeObj), nchar))
    assertthat::assert_that(!(minNameLen == 0),
                            msg = "All dataframes in dgeObj must be named (it must be a named list.)")

    # Set default columns
    if (missing(includeColumns)) {
        includeColumns <- colnames(dgeObj[[1]])
    }

    # Find the common set of columns present in all dataframes
    commonColumns <- colnames(dgeObj[[1]])
    for (i in 2:length(dgeObj))
        commonColumns <- intersect(commonColumns, colnames(dgeObj[[i]]))

    # Make sure user-requested columns are present
    if (!all(includeColumns %in% commonColumns)) {
        warning("Some requested columns are not present in all dataframes.")
    }
    commonColumns <- intersect(commonColumns, includeColumns)

    # Does the rownameColumn exist in df1?
    if (!rownameColumn %in% colnames(dgeObj[[1]])) {
        # Move rownames to rownameColumn
        dgeObj <- lapply(dgeObj, rownames_to_column, var = rownameColumn)
        commonColumns <- c(rownameColumn, commonColumns)
    }

    # Reduce all dataframes to the selected columns
    # This also insures same column order in each df
    dgeObj <- lapply(dgeObj, select, commonColumns)

    # Add a contrast name column to each DF
    for (name in names(dgeObj)) {
        dgeObj[[name]]["Contrast"] <- name
    }

    # Now merge the dataframes vertically
    dgeObj <- dplyr::bind_rows(dgeObj)

    return(dgeObj)
}
