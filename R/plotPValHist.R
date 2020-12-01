#' Plot histogram analysis of p-value distributions
#'
#' Generate a facet plot (or optionally individual plots) from a dataframe of
#' numbers.  Intended to perform histogram analysis of p-value distributions,
#' but should be useful for any dataframe of numeric columns.
#'
#' @param P.Val A matrix or dataframe of numeric data; col = samples
#' @param facet Set to FALSE to print individual plots instead of a faceted plot. (Default = TRUE)
#' @param savePlot TRUE saves the plot(s) to .PNG files.
#' @param fileNames Pass a single file name for a faceted plot to save the
#'        plot to a file.  Pass a list of file names if facet = FALSE (length must equal
#'        sample count).
#' @param binWidth Range is always 0-1 for p-values. (Default = 0.02)
#' @param baseFontSize Set the base font size for the plot using theme_grey. (Default = 12)
#' @param facetFontSize Control font size on the individual plot headers
#'   (default = NULL). 10 seems good for knitr output (dependent on length of
#'   sample titles). Bigger (e.g. 14) works better for PPT.
#' @param alpha Set the transparency. (Default = 0.6)
#' @param color Color for the histogram outline. (Default = "dodgerblue3")
#' @param fill Fill color for the histogram. (Default = "dodgerblue3")
#'
#' @return A ggplot2 object if facet = TRUE or a list of plots if facet = FALSE. (Default = TRUE)
#'
#' @examples
#' \dontrun{
#'    # Print to console using all defaults
#'    MyPvalMatrix <- extractCol(getType(myDGEobj, "topTable"), "P.Value")
#'    plotPvalHist(MyPvalMatrix)
#'
#'    # Print to a PNG with some custom arguments
#'    myplot <- plotPvalHist(MyPValMatrix,
#'                           savePlot = TRUE,
#'                           fileNames = "MyPlot.PNG",
#'                           facetFontSize = 14)
#' }
#'
#' @import ggplot2
#' @importFrom grDevices png dev.off
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#'
#' @export
plotPvalHist <- function(P.Val,
                         facet = TRUE,
                         savePlot = FALSE,
                         fileNames = NULL,
                         binWidth = 0.02,
                         baseFontSize = 12,
                         facetFontSize = NULL,
                         alpha = 0.6,
                         color = "dodgerblue3",
                         fill = "dodgerblue3") {

    if (is.matrix(P.Val)) {
        P.Val %<>% as.data.frame
    }

    NumSamples <- ncol(P.Val)
    SampNames <- colnames(P.Val)

    # Set up Tall format
    P.Val$GeneID = rownames(P.Val)
    P.Val %<>% tidyr::gather(key = "Levels", value = "Pval", -GeneID)

    if (facet) {
        numcol <- 3
        numrow <- (NumSamples / numcol) %>% ceiling
        if (is.null(fileNames)) {
            fileNames <- "PValueHistFacet.png"
        }

        Hist_Pval_Facet <- ggplot2::ggplot(data = P.Val, aes(x = Pval)) +
            ggplot2::geom_histogram(alpha = alpha,
                                    fill = fill,
                                    color = color,
                                    binwidth = binWidth) +
            ggplot2::xlab("P-value") +
            ggplot2::ylab("Count") +
            ggtitle("P-value Histograms") +
            ggplot2::scale_fill_brewer(palette = "Set1") +
            ggplot2::facet_wrap(~Levels, nrow = numrow, scales = "free") +
            theme_grey() + baseTheme(baseFontSize)

        if (!is.null(facetFontSize)) {
            Hist_Pval_Facet <- Hist_Pval_Facet +
                theme(strip.text.x = element_text(size = facetFontSize,
                                                  colour = "red", angle = 0))
        }

        if (savePlot) {
            grDevices::png(filename = fileNames[1], width = 8, height = 6, units = 'in', res = 300)
            print(Hist_Pval_Facet)
            invisible( grDevices::dev.off() )
        }
        return(Hist_Pval_Facet)

    } else {
        if (!is.null(fileNames)) {
            if (length(fileNames) != NumSamples) {
                stop("Number of fileNames does not match column count!")
            } else {
                fileNames = SampNames
                fileNames = gsub(":", "_", fileNames)
            }
        }

        # Run off the plots
        plotlist = list()
        for (i in 1:NumSamples) {
            if (!is.null(fileNames)) {
                f <- fileNames[i]
            }
            s <- SampNames[i]
            MyPVal <- dplyr::filter(P.Val, grepl(s, Levels))

            Hist_Pval <- ggplot2::ggplot(data = MyPVal, aes(x = Pval)) +
                ggplot2::geom_histogram(alpha = alpha,
                                        fill = fill,
                                        color = color,
                                        binwidth = binWidth) +
                ggplot2::xlab("P-value") +
                ggplot2::ylab("Count") +
                ggplot2::ggtitle(paste("P-value Histogram\n", s)) +
                theme_grey() + baseTheme(baseFontSize)

            plotlist[[i]] = Hist_Pval

            if (savePlot) {
                grDevices::png(filename = paste(f, ".png", sep = ""), width = 8, height = 6, units = 'in', res = 300)
                print(Hist_Pval)
                invisible( grDevices::dev.off() )
            }
        }
        return(plotlist)
    }
}
