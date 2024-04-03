#' DirectInput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DirectInput_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h3("Input a slug:"),
    textInput(NS(id,"slug"), "", "")
  )
}

#' DirectInput Server Functions
#'
#' @noRd
mod_DirectInput_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    return(reactive(input$slug))
  })
}

## To be copied in the UI
# mod_DirectInput_ui("DirectInput_1")

## To be copied in the server
# mod_DirectInput_server("DirectInput_1")
