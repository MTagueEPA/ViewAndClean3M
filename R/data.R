#' File IDs
#'
#' The first is the fileids dataset.  This is used by the document pulling
#' functionality, to map document slugs identified to Clowder ids, to allow for
#' pulling the pdf from Clowder.
#'
#' @name fileids
#' @keywords dataset
#' @docType data
#' @format A data frame containing 6 columns, including document name (and thus slug),
#' Clowder id, size, and more.
"fileids"

#' Filedf
#'
#' The other two datasets are the OCR outputs for the pdf files.  Both are data
#' frames containing document slugs and string outputs, to be used by the document
#' search function.  The filedf contains just this.
#'
#' @name filedf
#' @keywords dataset
#' @docType data
#' @format A data frame containing 2 columns - document slug, and string copy of
#' the OCR output for the document
"filedf"

#' Pagedf
#'
#' OCR outputs for the pdf files.  Contains a third column for page number, with
#' string outputs split by page.
#'
#' @name pagedf
#' @keywords dataset
#' @docType data
#' @format A data frame containing 2 columns - document slug, page number, and
#' string copy of the OCR output for the document
"pagedf"
