#' Function  OmicsoftToDgeObj
#'
#' Build a DGEobj from Omicsoft output or tabbed text files.
#'
#' User provides separate tab-delimited text files for row (gene) annotation,
#' column (sample) annotation, and "assays" (matrices of row by columns, e.g. counts, FPKM, etc).
#'
#' Two global variables hold the names of the Omicsoft data files: .geneData
#' and .isoformData.
#'
#' \strong{Data Requirements:}
#'
#' Assay data should have row names (sequence ids) and column names (sample IDs).
#' Sequence annotation should also have the same row names as assays.
#'
#' If possible, the sequence annotation should include chromosome position data (chr,
#' start, end, strand).
#'
#' Sample annotation is one row for each column in the count table.
#' rownames(sampleAnnotation) == colnames(counts).
#'
#' Function DGEobj::annotateDGEobj provides an easier way than the customAttr argument
#' here.  annotateDGEobj() reads key/value pairs from a text file to define
#' attributes.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords Omicsoft, DGEobj, RNA-Seq, Data loading
#'
#' @param path File path for the three data files (Default = "./")
#' @param counts A text file name for count data (gene rows by sample columns) (required)
#'  [Default = "RNA-Seq.Count.Table.txt"]
#' @param seqAnnotation  File name for Gene, isoform or exon level (row) annotation (required)
#'  [Default = "RNA-Seq.Count.Annotation.txt"]
#' @param design File name for sample annotation with expt factors and other sample-associated
#'     data (required) [Default = RNA-Seq.Design.txt"]
#' @param level One of "gene", "isoform", "exon" (required) [Default = "gene"]
#' @param source Default = "Omicsoft.  Change if your data is from elsewhere.
#' @param customAttr A named list of custom attributes to attach to the DGEobj;
#'    Optional but highly encouraged.  Suggestions: list(PID = "20170101-0001",
#'    Genome = "Mouse.B38", GeneModel = "Ensembl.R84")
#' @param gz Default = FALSE. If TRUE, adds ".gz" to a filename in the DGEobj attributes to
#'    support this type of file.
#'
#' @return A DGEobj
#'
#' @examples
#' \dontrun{
#'    # Defaults set for an omicsoft dataset:
#'    MyDGEobj <- OmicsoftToDgeObj(customAttr = list(PID = "20170101-0001",
#'                                                   Genome = "Mouse.B38",
#'                                                   GeneModel = "Ensembl.R84")
#'
#'    # Data files from somewhere else (not Omicsoft)
#'    MyDGEobj <- OmicsoftToDGEobj(MyCounts,
#'                                 MyGeneAnnotation,
#'                                 MyDesign,
#'                                 level = "gene",
#'                                 customAttr = list(PID = "20170101-0001",
#'                                                   Genome = "Mouse.B38",
#'                                                   GeneModel = "Ensembl.R84")
#'                )
#' }
#'
#' @importFrom stringr str_c
#' @importFrom DGEobj initDGEobj
#' @importFrom utils packageVersion read.table
#' @importFrom assertthat assert_that
#'
#' @export
OmicsoftToDgeObj <- function(counts = "RNA-Seq.Count.Table.txt",
                             seqAnnotation = "RNA-Seq.Count.Annotation.txt",
                             design = "RNA-Seq.Design.txt",
                             level = "gene",
                             source = "Omicsoft",
                             path = ".",
                             customAttr,
                             gz = FALSE){

    message("OmicsoftToDGEobj is deprecated as of 0.9.56. Use textToDgeObj instead.")
    #add support for gzipped files
    if (gz == TRUE) {
        gz <- ".gz"
    } else {
        gz <- ""
    }
    counts <- stringr::str_c(counts, gz)
    seqAnnotation <- stringr::str_c(seqAnnotation, gz)
    design <- stringr::str_c(design, gz)

    # Change default filenames if not given and level = isoform
    if (tolower(level) == "isoform") {
        if (missing(counts)) {
            counts <- stringr::str_c("RNA-Seq.Transcript_Count.Table.txt", gz)
        }

        if (missing(seqAnnotation)) {
            seqAnnotation <- stringr::str_c("RNA-Seq.Transcript_Count.Annotation.txt", gz)
        }

        if (missing(design)) {
            design <- stringr::str_c("RNA-Seq.Design.txt", gz)
        }
    }

    # Txt2DF helper function
    Txt2DF <- function(filename) {
        # Configured to read Omicsoft .txt files correctly capturing GeneIDs as rownames
        if (file.exists(filename)) {
            df = read.table(filename, sep = "\t", stringsAsFactors = FALSE,
                            header = TRUE, row.names = 1, comment.char = "",
                            quote = "", na.strings = c("NA", "."),
                            check.names = TRUE)
            return(df)
        } else {
            warning(paste("Warning: File = ", filename, "not found."))
            return(-1)
        }
    }

    # Get the data
    countData <- Txt2DF(file.path(path, counts))
    seqData <- Txt2DF(file.path(path, seqAnnotation))
    designData <- Txt2DF(file.path(path, design))
    rownames(designData) <- make.names(rownames(designData))

    # Add source to customAttr
    if (missing(customAttr)) {
        customAttr <- list(source = "Omicsoft")
    } else {
        assertthat::assert_that("list" %in% class(customAttr),
                                msg = "customAttr must be of class 'list'.")
        customAttr$source <- source
    }

    # Add DGE.Tools Version info
    customAttr$DGEtools <- packageVersion("DGEtools")
    customAttr$DGEobj <- packageVersion("DGEobj")

    # Build the DgeObj
    DgeObj <- DGEobj::initDGEobj(counts = countData,
                                 rowData = seqData,
                                 colData = designData,
                                 level,
                                 customAttr = customAttr)

    return(DgeObj)
}
