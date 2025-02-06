#' PDFPull UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import stringr
#' @import writexl
#' @importFrom shiny NS tagList
#' @import readxl
#' @importFrom utils download.file
mod_PDFPull_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      useShinyjs(),
      uiOutput(NS(id,"pdfviewer")),
      DT::DTOutput(NS(id, "table")),
      textOutput(NS(id,"blurb")),
      downloadButton(NS(id,"save"),"Save Table as Excel")
    )
  )
}

#' PDFPull Server Functions
#'
#' @noRd
mod_PDFPull_server <- function(id,slug,group){
  moduleServer( id, function(input, output, session){
    loadRData<-function(filename){
      load(paste("data",filename,sep="/"))
      base::get(ls()[ls() != "filename"])
    }

    download_file_from_api <- function(id, filename,apikey){
      if (substr(id,1,5) != "https") {
        id = paste0("https://clowder.edap-cluster.com/api/files/",id)
      }
      id=paste0(id,"/blob")
      out <- tryCatch({
        utils::download.file(url=id,
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

    downloader<-reactiveValues(df = data.frame())
    hide(id = "pdfviewer")
    hide(id = "table")
    hide(id = "blurb")
    hide(id = "save")

    notable<-modalDialog(
      textOutput("There is not currently a table stored.  If you see this dialog,
                 please reach out to Mitchell, as this should never be reached."),
      title = "No Table"
    )

    observeEvent(slug(), {
      if (!is.na(slug()[1])) {
        if (group() == "3M") {
          show(id = "pdfviewer")
          hide(id = "table")
          hide(id = "blurb")
          hide(id = "save")
          fileindex<-reactive(which(substr(fileids$filename,1,4)==slug()[1]))
          filename<-reactive(fileids$filename[fileindex()])
          fileid<-reactive(fileids$id[fileindex()])
          if (length(filename()) != 0 & !file.exists(paste0("inst/app/www/",filename()))){
            download_file_from_api(fileid(),filename(),Sys.getenv("apikey"))
          }
          output$pdfviewer <- renderUI({
            tags$iframe(style="height:485px; width:100%", src=paste0("www/",filename()))
          })
        } else {
          hide(id = "table")
          hide(id = "pdfviewer")
          hide(id = "blurb")
          hide(id = "save")
          this.slug<-slug()[1]
          fileindex<-which(DCAPfiledf$file==this.slug)
          filetype<-DCAPfiledf$type[fileindex]
          if (length(filetype) != 0){
            if (filetype == "table") {
              these.rows<-slug()[2]
              these.rows<-as.numeric(str_split(these.rows," ")[[1]])
              thistable<-loadRData(this.slug)
              garb.columns<-DCAPfiledf$garbage[fileindex]
              garb<-FALSE
              if (garb.columns != "0") {
                garb<-TRUE
                garb.columns<-as.numeric(str_split(garb.columns," ")[[1]])
                garbnames<-colnames(thistable)[garb.columns]
              }
              thistable<-thistable[these.rows,]
              downloader$df<-as.data.frame(thistable)
              if (garb) {thistable<-thistable[,-garb.columns]}
              thistable<-DT::datatable(thistable, options = list(scrollX = TRUE, columnDefs = list(list(
              ))))
              output$table <- DT::renderDT(thistable)
              garbnames<-paste(garbnames,collapse=", ")
              output$blurb <- renderText(paste0("The following columns are not included
                                               for spacing concerns: ",garbnames,
                                               ". Please download the table to investigate
                                               those columns."))
              show(id = "table")
              show(id = "blurb")
              show(id = "save")
            } else {
              filename<-this.slug

              if (length(filename) != 0 & !file.exists(paste0("inst/app/www/",filename))){
                url<-DCAPfiledf$url[which(DCAPfiledf$file==filename)]

                download_file_from_api(url,filename,Sys.getenv("apikey"))

              }

              output$pdfviewer <- renderUI({
                tags$iframe(style="height:485px; width:100%", src=paste0("www/",filename))
              })
              show(id = "pdfviewer")
            }
          }
        }
      }
    })
    output$save<-downloadHandler(
      filename=function(){
        paste0(substr(slug()[1],1,nchar(slug()[1])-4),".xlsx")
      },
      content=function(file) {
        write_xlsx(downloader$df,file)
      }
    )
  })
}

## To be copied in the UI
# mod_PDFPull_ui("PDFPull_1")

## To be copied in the server
# mod_PDFPull_server("PDFPull_1")
