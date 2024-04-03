#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE
#' @import shiny
#' @import rpdf
#' @importFrom DBI dbConnect
#' @importFrom DBI dbReadTable
#' @noRd
app_server <- function(input, output, session) {
  fpval<-mod_FrontPage_server("fp")
  username<-reactive(fpval()[2])
  mod_Viewer_server("vi",username)
  observeEvent(fpval(), {
               if (fpval()[1] == "vi") {
                 updateTabsetPanel(session,"overall","vi")
               }})
}
