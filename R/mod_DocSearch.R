#' DocSearch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import stringr
#' @import dplyr
#' @importFrom DBI dbConnect
#' @importFrom DBI dbReadTable
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DBI sqlInterpolate
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbClearResult
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom RPostgres Postgres
#' @importFrom shiny NS tagList
mod_DocSearch_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    h3("Input a term to search for.  Use ; to split multiple terms."),
    textInput(NS(id,"term"), "", ""),
    checkboxInput(NS(id, "page"), "Page Separated Search?"),
    radioButtons(NS(id, "andor"), "Multiple Terms:", c("AND" = "and", "OR" = "or"), inline=TRUE),
    actionButton(NS(id, "search"),"Begin Search"),
    actionButton(NS(id, "pullsaved"),"Show Saved Results"),
    DTOutput(NS(id,"table")),
    actionButton(NS(id, "newsaved"),"Save Results")
  )
}

#' DocSearch Server Functions
#'
#' @noRd

mod_DocSearch_server <- function(id,username){
  moduleServer( id, function(input, output, session){
    fastsearch<-function(term,andor,page=FALSE) {
      if (page) {df = pagedf} else {df = filedf}

      #Standardize the termlist. Create two versions of the termlist, one which flattens all terms to a single
      #word, and one which replaces all special characters with spaces. This is to both match the text lists
      #later and to potentially catch for OCR garbage eating single spaces. Also captures the length of each
      #term, for ease of searching later.
      term<-tolower(term)
      if (grep(";",term)) {
        multi<-TRUE
        termvec<-str_split(term,";")[[1]]
        termsflat<-c()
        termsspace<-c()
        termslength<-c()
        for (i in 1:length(termvec)) {
          splitlist<-str_split(termvec[i],"[^a-zA-Z0-9]")[[1]]
          #Take the split vector for a term, find it's length, then create a flattened and a spaced term from it.
          these<-which(splitlist %in% c("", " "))
          if (length(these != 0)){temp<-splitlist[-these]}
          termslength[i]<-length(temp)
          tempstrflat<-temp[1]
          tempstrspace<-temp[1]
          if (termslength[i] > 1) {for (j in 2:termlength) {
            tempstrflat<-paste(tempstrflat,temp[j],sep="")
            tempstrspace<-paste(tempstrspace,temp[j])
          }}
          termsflat[i]<-tempstrflat
          termsspace[i]<-tempstrspace
        }
      } else {
        multi<-FALSE
        #This little line here says "split this string vector on alphanumeric characters." Creates a list of
        #vectors to easily iterate through
        splitlist<-str_split(term,"[^a-zA-Z0-9]")[[1]]
        #Take the split vector for a term, find it's length, then create a flattened and a spaced term from it.
        these<-which(splitlist %in% c("", " "))
        if (length(these != 0)){temp<-splitlist[-these]}
        termlength<-length(temp)
        tempstrflat<-temp[1]
        tempstrspace<-temp[1]
        if (termlength > 1) {for (j in 2:termlength) {
          tempstrflat<-paste(tempstrflat,temp[j],sep="")
          tempstrspace<-paste(tempstrspace,temp[j])
        }}
        termflat<-tempstrflat
        termspace<-tempstrspace
      }

      #Create the df if necessary, and create the results based on options - "Page" if TRUE, "NumCheck" if TRUE,
      #and Term in cases where the termlist has more than one option, to show which term a specific check hit.
      columns<-c("Slug")
      if (page) {columns<-c(columns,"Page")}
      if (multi) {columns<-c(columns,"Most Common")}
      columns<-c(columns,"Hits")
      results<-as.data.frame(matrix(nrow=0,ncol=length(columns)))
      colnames(results)<-columns

      #Now the fun begins. Go by rows of the df, since they're already split as needed.
      #Run the same string splitting as above for each string, then clean up and
      #lowercase the results.
      progn<-nrow(df)
      for (i in 1:nrow(df)) {
        if (i %% 50 == 0) {incProgress(50/progn)}
        text<-df$Text[i]
        if (is.na(text) | length(text) == 0 | nchar(text) == 0) {next}
        text<-str_split(text,"[^a-zA-Z0-9]")[[1]]
        text<-text[which(text!="")]
        text<-tolower(text)
        #Look if individual words are in the flattened term list.
        if (multi) {
          n<-c()
          for (j in 1:length(termsflat)) {
            term<-termsflat[j]
            hits<-grep(term,text)
            n[j]<-length(hits)
            if (termslength[j] > 1 & termslength[j] < length(text)) {
              textvec<-c()
              for (k in termlength:length(text)) {
                index<-k-termlength+1
                temp<-text[index:k]
                textvec[index]<-paste(temp,collapse=" ")
              }
              n[j]<-n[j]+length(grep(termsspace[j],textvec))
            }
          }
          if (andor == "and") {
            if (sum(n > 0) == length(n)) {
              tempr<-c(df$Slug[i])
              if (page) {tempr<-cbind(tempr,df$Page[i])}
              this.one<-which.max(n)
              tempr<-cbind(tempr,termvec[this.one],min(n))
              results<-rbind(results,tempr)
            }
          } else {
            if (sum(n > 0)) {
              tempr<-c(df$Slug[i])
              if (page) {tempr<-cbind(tempr,df$Page[i])}
              this.one<-which.max(n)
              tempr<-cbind(tempr,termvec[this.one],sum(n))
              results<-rbind(results,tempr)
            }
          }
        } else {
          hits<-grep(termflat,text)
          n<-length(hits)
          #This segment works similarly to the last one, but aims to search specifically
          #for multi-word terms. Each search is dependent on the number of words in a term,
          #so iterate over unique term lengths. The flattened term list handles all one-word
          #terms, so we can skip them.
          if (termlength > 1 & termlength < length(text)) {
            textvec<-c()
            for (j in termlength:length(text)) {
              index<-j-termlength+1
              temp<-text[index:j]
              textvec[index]<-paste(temp,collapse=" ")
            }
            hits<-grep(termspace,textvec)
            n<-n+length(hits)
          }
          if (n > 0) {
            tempr<-c(df$Slug[i])
            if (page) {tempr<-cbind(tempr,df$Page[i])}
            tempr<-c(tempr,n)
            results<-rbind(results,tempr)
          }
        }
      }
      #And return results!
      colnames(results)<-columns
      results<-distinct(results)
      results$Hits<-as.numeric(results$Hits)
      return(results[order(results$Hits,decreasing=TRUE),])
    }

    ns <- session$ns
    conbool<-FALSE
    tryCatch({
      con<-dbConnect(RPostgres::Postgres(),
                     host="ccte-postgres-res.dmap-prod.aws.epa.gov",
                     port=5432,
                     user=Sys.getenv("sqladminname"),
                     password=Sys.getenv("sqladminpass"),
                     dbname="res_viewandclean3m")
      conbool<-TRUE
    }, warning = function(w) {conbool<-FALSE}, error = function(e) {conbool<-FALSE}
    )
    nosave<-modalDialog(
      textOutput("No saved results!  Please make a document search and save it first."),
      title = "No Saved Results"
    )
    savedres<-modalDialog(
      DTOutput(ns("savedtable")),
      actionButton(ns("loadtable"),"Load Selected Table"),
      title = "Saved Search Results"
    )
    hide(id = "newsaved")
    if (!conbool) {(hide(id = "pullsaved"))}

    holder<-reactiveValues(df = data.frame())
    observeEvent(input$search,{
      thistable<-withProgress(fastsearch(input$term, input$andor, page = input$page), value = 0,
                              message = "Searching Document Set")
      holder$df<-as.data.frame(thistable)
      output$table<-renderDT(thistable,
                             rownames=FALSE,
                             selection='single')
      if (conbool) {show(id = "newsaved")}
    })
    observeEvent(input$pullsaved,{
      restable<-dbReadTable(con,"results")
      thistable<-restable[which(restable$username==username()),]
      if (nrow(thistable)==0) {
        showModal(nosave)
      } else {
        searches<-max(thistable$searchid)
        final<-as.data.frame(matrix(nrow=0,ncol=4))
        for (i in 1:searches){
          thattable<-thistable[which(thistable$searchid==i),]
          pagebool<-thattable$searchpage[1]
          term<-thattable$searchterm[1]
          multi<-thattable$searchmulti[1]
          docs<-length(unique(thattable$slug))
          hits<-sum(thattable$hits)
          final<-rbind(final,c(term,pagebool,multi,docs,hits))
        }
        colnames(final)<-c("Search Term","Page?","Multi-Term?","Documents","Total Hits")
        output$savedtable<-renderDT(final,rownames=FALSE,selection='single')
        showModal(savedres)
      }
    })
    observeEvent(input$loadtable,{
      restable<-dbReadTable(con,"results")
      id<-input$savedtable_cell_clicked$row
      thisone<-restable[which(restable$searchid==id),]
      if (thisone$searchpage[1] & thisone$searchmulti[1] != "none") {
        final<-thisone[,6:9]
        holder$df<-final
        output$table<-renderDT(final,rownames=FALSE,selection='single')
      } else if (thisone$searchpage[1]) {
        final<-thisone[,c(6,7,9)]
        holder$df<-final
        output$table<-renderDT(final,rownames=FALSE,selection='single')
      } else if (thisone$searchmulti[1] != 1) {
        final<-thisone[,c(6,8,9)]
        holder$df<-final
        output$table<-renderDT(final,rownames=FALSE,selection='single')
      } else {
        final<-thisone[,c(6,9)]
        holder$df<-final
        output$table<-renderDT(final,rownames=FALSE,selection='single')
      }
      removeModal()
    })
    observeEvent(input$newsaved,{
      restable<-dbReadTable(con,"results")
      thistable<-restable[which(restable$username==username()),]
      n<-nrow(holder$df)
      if (nrow(thistable) == 0) {
        id<-rep(1,n)
      } else {
        id<-rep(max(thistable$searchid)+1,n)
      }
      if("Page" %in% colnames(holder$df)) {
        pagebool<-rep(TRUE,n)
        page<-holder$df$Page
      } else {
        pagebool<-rep(FALSE,n)
        page<-rep(1,n)
      }
      if (grep(";",input$term)) {
        multi<-rep(input$andor,n)
        common<-holder$df$`Most Common`
      } else {
        multi<-rep("none",n)
        common<-rep("none",n)
      }
      send<-cbind(rep(username(),n),id,rep(input$term,n),pagebool,multi,holder$df$Slug,page,common,holder$df$Hits)
      colnames(send)<-colnames(restable)
      withProgress(for(i in 1:n){
        incProgress(1/n)
        row<-send[i,]
        querstring = "INSERT INTO results (username,searchid,searchterm,searchpage,searchmulti,slug,page,common,hits)
                      VALUES (?one,?two,?three,?four,?five,?six,?seven,?eight,?nine)"
        query <- sqlInterpolate(con, querstring, one=row[1], two=row[2],three=row[3],four=row[4],five=row[5],six=row[6],seven=row[7],eight=row[8],nine=row[9])
        rs<-dbSendQuery(con,query)
        dbClearResult(rs)
      },value=0,message="Saving Search Result")
    })
    output$slug<-reactive(holder$df[input$table_cell_clicked$row,1])
  })
}


## To be copied in the UI
# mod_DocSearch_ui("DocSearch_1")

## To be copied in the server
# mod_DocSearch_server("DocSearch_1")
