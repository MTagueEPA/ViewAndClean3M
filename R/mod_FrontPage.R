#' FrontPage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom DBI dbConnect
#' @importFrom DBI dbReadTable
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbClearResult
#' @importFrom DBI sqlInterpolate
#' @importFrom RPostgres Postgres
#' @importFrom shiny NS tagList
mod_FrontPage_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    textInput(NS(id,"userinput"),"Username:",""),
    passwordInput(NS(id,"passinput"),"Password:",""),
    actionButton(NS(id,"login"), "Log In"),
    actionButton(NS(id,"creation"), "Create An Account"),
    actionButton(NS(id,"guestlogin"), "Continue As Guest")
  )
}

#' FrontPage Server Functions
#'
#' @noRd
mod_FrontPage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    conbool<-FALSE
    tryCatch({
      con<-dbConnect(RPostgres::Postgres(),
                     host="ccte-postgres-res.dmap-prod.aws.epa.gov",
                     port=5432,
                     user=Sys.getenv("sqladminname"),
                     password=Sys.getenv("sqladminpass"),
                     dbname="res_viewandclean3m")
      logins<-dbReadTable(con,"logins")
      conbool<-TRUE
    }, warning = function(w) {conbool<-FALSE}, error = function(e) {conbool<-FALSE}
    )
    if (!conbool) {
      hide(id = "login")
      hide(id = "creation")
    }
    retVal<-reactiveValues(navs=c("",""))
    loginfail<-modalDialog(
      "Login not recognized!",
      title = "Login Failure",
    )
    accountmake<-modalDialog(
      "Please create an account:",
      title = "Account Creation",
      textInput(ns("usermake"),"Username:"),
      passwordInput(ns("passmake1"),"Password:"),
      passwordInput(ns("passmake2"),"Confirm Password:"),
      actionButton(ns("createconfirm"), "Create Account")
    )
    passfail<-modalDialog(
      "Passwords Do Not Match, please resubmit!",
      title = "Login Failure",
      easyClose = FALSE,
      footer = actionButton(ns("backto1"),label="Dismiss")
    )
    already<-modalDialog(
      "That username has already been taken!",
      title = "Username Taken",
      easyClose = FALSE,
      footer = actionButton(ns("backto2"),label="Dismiss")
    )
    maxlength<-modalDialog(
      "Maximum length for these fields is 30!",
      title = "Too Long",
      easyClose = FALSE,
      footer = actionButton(ns("backto3"),label="Dismiss")
    )
    observeEvent(input$login,{
      if (length(which(logins$username==input$userinput & logins$password == input$passinput)) != 0) {
        dbDisconnect(con)
        retVal$navs = c("vi",input$userinput)
      } else {
        showModal(loginfail)
      }
    })
    observeEvent(input$guestlogin, {
      if (conbool) {dbDisconnect(con)}
      retVal$navs = c("vi","Guest")
    })
    observeEvent(input$creation,{showModal(accountmake)})
    observeEvent(input$backto1,{showModal(accountmake)})
    observeEvent(input$backto2,{showModal(accountmake)})
    observeEvent(input$backto3,{showModal(accountmake)})
    observeEvent(input$createconfirm,{
      if (input$passmake1 != input$passmake2){
        showModal(passfail)
      } else if (input$usermake %in% logins$username) {
        showModal(already)
      } else if (length(input$usermake) > 30 | length(input$passmake1) > 30 | length(input$passmake2) > 30) {
        showModal(maxlength)
      } else {
        querstring = "INSERT INTO logins (username,password,cleaning)
                      VALUES (?username,?password,FALSE)"
        query <- sqlInterpolate(con, querstring, username=input$usermake, password=input$passmake1)
        rs<-dbSendQuery(con,query)
        dbClearResult(rs)
        dbDisconnect(con)
        retVal$navs = c("vi",input$usermake)
        removeModal()
      }
    })
    return(reactive(retVal$navs))
  })
}


## To be copied in the UI
# mod_FrontPage_ui("FrontPage_1")

## To be copied in the server
# mod_FrontPage_server("FrontPage_1")
