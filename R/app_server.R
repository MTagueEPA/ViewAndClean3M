#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import rpdf
#' @noRd
app_server <- function(input, output, session) {
  mod_PDFPull_server("amiwork")
}
