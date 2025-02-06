#' DCAP Docs UI Function
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
mod_DCAPDocs_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    h3("Input a DTXSID to search for."),
    textInput(NS(id,"chem"), "", ""),
    h3("Input a term to search for.  Use ; to split multiple terms."),
    textInput(NS(id,"term"), "", ""),
    radioButtons(NS(id, "andor"), "Multiple Terms:", c("AND" = "and", "OR" = "or"), inline=TRUE),
    actionButton(NS(id, "search"),"Begin Search"),
    #actionButton(NS(id, "pullsaved"),"Show Saved Results"),
    DTOutput(NS(id,"table")),
    #actionButton(NS(id, "newsaved"),"Save Results")
  )
}

#' DCAPDocs Server Functions
#'
#' @noRd

mod_DCAPDocs_server <- function(id,username){
  moduleServer( id, function(input, output, session){
    loadRData<-function(filename){
      load(paste("data",filename,sep="/"))
      base::get(ls()[ls() != "filename"])
    }
    onesearch<-function(chem) {
      chem<-toupper(chem)
      filenames = unique(DCAPdf$document_name[which(DCAPdf$dtxsid==chem)])
      these.ones<-filenames[which(filenames %in% zipdf$zip)]
      if (length(these.ones)!=0) {
        filenames<-filenames[which(!filenames %in% zipdf$zip)]
        morenames<-zipdf$file[which(zipdf$zip %in% these.ones)]
        filenames<-c(filenames,morenames)
      }
      return(filenames)
    }
    filesearch<-function(term,filenames,andor) {
      #Standardize the termlist. Create two versions of the termlist, one which flattens all terms to a single
      #word, and one which replaces all special characters with spaces. This is to both match the text lists
      #later and to potentially catch for OCR garbage eating single spaces. Also captures the length of each
      #term, for ease of searching later.
      df<-DCAPfiledf[which(DCAPfiledf$file %in% filenames),]
      df<-df[which(df$type!="table"),]
      if (term == "") {
        columns<-c("File","Type","Hits")
        results<-as.data.frame(matrix(nrow=0,ncol=length(columns)))
        for (i in 1:nrow(df)) {
          results<-rbind(results,c(df$file[i],df$type[i],0))
        }
        colnames(results)<-columns
        results<-distinct(results)
      } else {
        term<-tolower(term)
        if (length(grep(";",term) != 0)) {
          multi<-TRUE
          termvec<-str_split(term,";")[[1]]
          termsflat<-c()
          termsspace<-c()
          termslength<-c()
          for (i in 1:length(termvec)) {
            splitlist<-str_split(termvec[i],"[^a-zA-Z0-9]")[[1]]
            #Take the split vector for a term, find it's length, then create a flattened and a spaced term from it.
            these<-which(splitlist %in% c("", " "))
            if (length(these)!=0){temp<-splitlist[-these]} else {temp<-splitlist}
            termslength[i]<-length(temp)
            tempstrflat<-temp[1]
            tempstrspace<-temp[1]
            if (termslength[i] > 1) {for (j in 2:termslength) {
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
          if (length(these != 0)){temp<-splitlist[-these]} else {temp<-splitlist}
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
        columns<-c("File","Type")
        if (multi) {columns<-c(columns,"Most Common")}
        columns<-c(columns,"Hits")
        results<-as.data.frame(matrix(nrow=0,ncol=length(columns)))
        colnames(results)<-columns
        if (nrow(df)==0) {return(results)}

        #Now the fun begins. Go by rows of the df, since they're already split as needed.
        #Run the same string splitting as above for each string, then clean up and
        #lowercase the results.
        progn<-nrow(df)
        for (i in 1:nrow(df)) {
          incProgress(1/progn)
          text<-df$string[i]
          type<-df$type[i]
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
                for (k in termslength[j]:length(text)) {
                  index<-k-termslength[j]+1
                  temp<-text[index:k]
                  textvec[index]<-paste(temp,collapse=" ")
                }
                n[j]<-n[j]+length(grep(termsspace[j],textvec))
              }
            }
            if (andor == "and") {
              if (sum(n > 0) == length(n)) {
                tempr<-c(df$file[i],type)
                this.one<-which.max(n)
                tempr<-c(tempr,termvec[this.one],min(n))
                results<-rbind(results,tempr)
              }
            } else {
              if (sum(n > 0)) {
                tempr<-c(df$file[i],type)
                this.one<-which.max(n)
                tempr<-c(tempr,termvec[this.one],sum(n))
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
              tempr<-c(df$file[i],type)
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
    }
    tablesearch<-function(term,filenames,chem,andor) {
      if (chem %in% dtxsidf$Parent_DTXSID) {
        chem<-unique(dtxsidf$DTXSID[which(dtxsidf$Parent_DTXSID==chem)])
      } else {chem<-c(chem)}

      df<-DCAPfiledf[which(DCAPfiledf$file %in% filenames),]
      df<-df[which(df$type=="table"),]

      if (term == "") {
        columns<-c("File","Type","Hits")
        results<-as.data.frame(matrix(nrow=0,ncol=length(columns)))
        colnames(results)<-columns
      } else {
        columns<-c("File","Type")
        if (length(grep(";",term) != 0)) {
          multi<-TRUE
          term<-str_split(term,";")[[1]]
        } else {multi<-FALSE}

        if (multi) {columns<-c(columns,"Most Common")}

        columns<-c(columns,"Hits")
        results<-as.data.frame(matrix(nrow=0,ncol=length(columns)))
        colnames(results)<-columns
      }
      n<-nrow(df)
      for (i in 1:n) {
        incProgress(1/n)
        file<-df$file[i]
        hits<-c()
        temp<-loadRData(file)

        if (df$string[i] == "True") {
          colind<-which(colnames(temp) %in% c("dtxsid","dsstox_substance_id"))
          rows<-which(temp[,colind] == chem)
        } else {rows<-1:nrow(temp)}
        if (term == "") {
          hitstr<-paste(rows,collapse=" ")
          results<-rbind(results,c(file,"table",hitstr))
        } else {
          for (j in rows) {
            if (multi) {
              tempres<-c()
              for (k in 1:length(term)) {
                tempres[k]<-any(str_detect(temp[j,],term[k]),na.rm=TRUE)
              }
              if (andor=="and") {
                if (all(tempres)) {hits<-c(hits,j)}
              } else {if (any(tempres)) {hits<-c(hits,j)}}
            } else {
              if (any(str_detect(temp[j,],term),na.rm=TRUE)) {hits<-c(hits,j)}
            }
          }
          if (!is.null(hits)){
            hitstr<-paste(hits,collapse=" ")
            if (multi) {results<-rbind(results,c(file,"table","",hitstr))} else {results<-rbind(results,c(file,"table",hitstr))}
          }
        }
      }

      colnames(results)<-columns
      return(results)
    }
    twosearch<-function(term,filenames,chem,andor) {
      if (length(filenames)==0) {return("None")}
      df1<-withProgress(filesearch(term,filenames,andor), value = 0, message = "Searching PDFs")
      df2<-withProgress(tablesearch(term,filenames,chem,andor), value = 0, message = "Searching Tables")
      results<-rbind(df1,df2)
      if (nrow(results) == 0) {return("aww")}
      if (ncol(results) == 4) {results<-results[,c(1,2,4)]}
      return(results)
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
      title = "No Saved Results",
      "No saved results!  Please make a document search and save it first."
    )
    nochem<-modalDialog(
      title = "No DTXSID",
      "This is not a DTXSID in the dataset!  If you're certain this is should be
      in, please reach out to Mitchell or Risa."
    )
    nohits<-modalDialog(
      title = "Zero results",
      "This search returned no results."
    )
    savedres<-modalDialog(
      DTOutput(ns("savedtable")),
      actionButton(ns("loadtable"),"Load Selected Table"),
      title = "Saved Search Results"
    )
    #hide(id = "newsaved")
    #if (!conbool) {(hide(id = "pullsaved"))}

    holder<-reactiveValues(df = data.frame())
    observeEvent(input$search,{
      andor<-input$andor
      chem<-input$chem
      nope<-FALSE
      if (!is.na(as.numeric(chem))){
        chem = paste("DTXSID",chem,sep="")
      } else if (!substr(chem,1,6) %in% c("DTXSID","dtxsid")) {
        showModal(nochem)
        nope<-TRUE
      }
      if (!nope) {
        term<-input$term
        term<-tolower(term)
        chem<-toupper(chem)
        thistable<-twosearch(term, onesearch(chem), chem, andor)
        if (any(thistable == "None")) {
          showModal(nochem)
        } else if (any(thistable == "aww")) {
          showModal(nohits)
        } else {
          holder$df<-as.data.frame(thistable)
          thistable[,1]<-substr(thistable[,1],1,25)
          output$table<-renderDT(thistable[,c(1,2)],
                                 rownames=FALSE,
                                 selection='single',
                                 width = 250)
          #if (conbool) {show(id = "newsaved")}
        }
      }
    })
    # observeEvent(input$pullsaved,{
    #   restable<-dbReadTable(con,"dcapresults")
    #   thistable<-restable[which(restable$username==username()),]
    #   if (nrow(thistable)==0) {
    #     showModal(nosave)
    #   } else {
    #     searches<-max(thistable$searchid)
    #     final<-as.data.frame(matrix(nrow=0,ncol=4))
    #     for (i in 1:searches){
    #       thattable<-thistable[which(thistable$searchid==i),]
    #       term<-thattable$searchterm[1]
    #       multi<-thattable$searchmulti[1]
    #       chem<-thattable$searchchem[1]
    #       docs<-length(unique(thattable$names))
    #       final<-rbind(final,c(chem,term,multi,docs))
    #     }
    #     colnames(final)<-c("Search Chem","Search Term","Multi-Term?","Documents")
    #     output$savedtable<-renderDT(final,rownames=FALSE,selection='single')
    #     showModal(savedres)
    #   }
    # })
    # observeEvent(input$loadtable,{
    #   restable<-dbReadTable(con,"dcapresults")
    #   id<-input$savedtable_cell_clicked$row
    #   thisone<-restable[which(restable$searchid==id),]
    #   final<-thisone[,c(6,7,8)]
    #   holder$df<-final
    #   output$table<-renderDT(final[,c(1,2)],rownames=FALSE,selection='single')
    #   removeModal()
    # })
    # observeEvent(input$newsaved,{
    #   restable<-dbReadTable(con,"dcapresults")
    #   thistable<-restable[which(restable$username==username()),]
    #   n<-nrow(holder$df)
    #   if (nrow(thistable) == 0) {
    #     id<-rep(1,n)
    #   } else {
    #     id<-rep(max(thistable$searchid)+1,n)
    #   }
    #   if (length(grep(";",input$term)) != 0) {
    #     multi<-rep(input$andor,n)
    #   } else {
    #     multi<-rep("none",n)
    #   }
    #   send<-cbind(rep(username(),n),id,rep(input$chem,n),rep(input$term,n),multi,holder$df$File,holder$df$Type,holder$df$Hits)
    #   colnames(send)<-colnames(restable)
    #   withProgress(for(i in 1:n){
    #     incProgress(1/n)
    #     row<-send[i,]
    #     querstring = "INSERT INTO dcapresults (username,searchid,searchchem,searchterm,searchmulti,names,types,rows)
    #                   VALUES (?one,?two,?three,?four,?five,?six,?seven,?eight)"
    #     query <- sqlInterpolate(con, querstring, one=row[1], two=row[2],three=row[3],four=row[4],five=row[5],six=row[6],seven=row[7],eight=row[8])
    #     rs<-dbSendQuery(con,query)
    #     dbClearResult(rs)
    #   },value=0,message="Saving Search Result")
    # })
    output$slug<-reactive(c(holder$df[input$table_cell_clicked$row,1],holder$df[input$table_cell_clicked$row,3]))
  })
}
