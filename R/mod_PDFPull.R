#' PDFPull UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import readxl
#' @importFrom utils download.file
mod_PDFPull_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      uiOutput(NS(id,"pdfviewer"))
    )
  )
}

#' PDFPull Server Functions
#'
#' @noRd
mod_PDFPull_server <- function(id,slug){
  moduleServer( id, function(input, output, session){
    download_file_from_api <- function(id, filename,apikey){
      out <- tryCatch({
        utils::download.file(url=paste0("https://clowder.edap-cluster.com/api/files/",id,"/blob"),
                             headers=c('X-API-Key'=apikey),
                             destfile=paste0("inst/app/www/",filename),
                             mode = "wb")
      }, error=function(e) {
        message(e)
        return(NULL)
      }
      )
      return(out)
    }

    fileindex<-reactive(which(substr(fileids$filename,1,4)==slug()))
    filename<-reactive(fileids$filename[fileindex()])
    fileid<-reactive(fileids$id[fileindex()])
    observeEvent(filename(),
      if (length(filename()) != 0 & !file.exists(paste0("inst/app/www/",filename()))){
        download_file_from_api(fileid(),filename(),Sys.getenv("apikey"))
    })
    output$pdfviewer <- renderUI({
      tags$iframe(style="height:485px; width:100%", src=paste0("www/",filename()))
    })
  })
}

## To be copied in the UI
# mod_PDFPull_ui("PDFPull_1")

## To be copied in the server
# mod_PDFPull_server("PDFPull_1")
