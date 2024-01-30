#' PDFPull UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PDFPull_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      h1("ViewAndClean3M"),
      sidebarPanel(
        tags$h3("Input a slug:"),
        textInput(NS(id,"slug"), "", ""),
      ),
      mainPanel(
        uiOutput(NS(id,'pdfviewer'))
      )
    )
  )
}

#' PDFPull Server Functions
#'
#' @noRd
mod_PDFPull_server <- function(id){
  moduleServer( id, function(input, output, session){
    files<-list.files("inst/app/www")
    pattern<-reactive(paste(input$slug,"_3M",sep=""))
    fileindex<-reactive(grep(pattern(),files))
    filename<-reactive(files[fileindex()])
    output$pdfviewer <- renderUI({
      tags$iframe(style="height:485px; width:100%", src=paste0("www/",filename()))
    })
  })
}

## To be copied in the UI
# mod_PDFPull_ui("PDFPull_1")

## To be copied in the server
# mod_PDFPull_server("PDFPull_1")
