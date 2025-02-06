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
      h1("Document Viewer"),
      sidebarPanel(
        tabsetPanel(
          id = NS(id,"searchpull"),
          tabPanel(
            title = 'DCAP Documents',
            value = ns('dctab'),
            mod_DCAPDocs_ui(NS(id,"dc"))
          ),
          tabPanel(
            title = '3M DocSearch',
            value = ns('dstab'),
            mod_DocSearch_ui(NS(id,"ds"))
          ),
          # tabPanel(
          #   title = '3M Slugs',
          #   value = ns('ditab'),
          #   mod_DirectInput_ui(NS(id,"di"))
          # )
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
    dcval<-mod_DCAPDocs_server("dc",username)
    slugf<-reactive(
      if (input$searchpull=="vi-ditab") {
        dival()
      } else if (input$searchpull == "vi-dctab") {
        dcval()
      } else {dsval()}
    )
    group<-reactive(
      if (input$searchpull == "vi-dctab") {"DCAP"} else {"3M"}
    )
    mod_PDFPull_server("pdf",slugf,group)
  })
}

## To be copied in the UI
# mod_Viewer_ui("Viewer_1")

## To be copied in the server
# mod_Viewer_server("Viewer_1")
