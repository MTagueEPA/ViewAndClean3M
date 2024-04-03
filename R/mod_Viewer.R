#' Viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Viewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      h1("ViewAndClean3M"),
      sidebarPanel(
        tabsetPanel(
          id = NS(id,"searchpull"),
          tabPanel(
            title = 'DS Tab',
            value = ns('dstab'),
            mod_DocSearch_ui(NS(id,"ds"))
          ),
          tabPanel(
            title = 'DI Tab',
            value = ns('ditab'),
            mod_DirectInput_ui(NS(id,"di"))
          )
        )
      ),
      mod_PDFPull_ui(NS(id,"pdf"))
    )
  )
}

#' Viewer Server Functions
#'
#' @noRd
mod_Viewer_server <- function(id,username){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    dival<-mod_DirectInput_server("di")
    dsval<-mod_DocSearch_server("ds",username)
    slugf<-reactive(
      if (input$searchpull=="vi-ditab") {
        dival()
      } else {dsval()}
    )
    mod_PDFPull_server("pdf",slugf)
  })
}

## To be copied in the UI
# mod_Viewer_ui("Viewer_1")

## To be copied in the server
# mod_Viewer_server("Viewer_1")
