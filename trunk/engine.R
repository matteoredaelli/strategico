#!/usr/bin/env Rscript
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Authors: L. Finos, M. Redaelli

library(RODBC)
source("strategico.config")

GetProjectConfig <- function(project.config.fileName="project.config") { #cerca il file nella cartella : getwd()
  conf=read.table(project.config.fileName, head=FALSE,sep=":",stringsAsFactors =FALSE,quote="\"")
                                        #e assegnazione dei valori indicati dal file ai parametri
  project.name <- conf$V2[conf$V1=="project.name"]
  connector.package <- conf$V2[conf$V1=="connector.package"]
  eval.package <- conf$V2[conf$V1=="eval.package"]
  eval(parse(text=paste("save=c(",conf$V2[conf$V1=="save"],")"),))
  
  keys <- conf$V2[.GetFieldsId(conf$V1,"key")]
  names(keys) <- .GetFields(conf$V1,"key")
  
  values <- conf$V2[.GetFieldsId(conf$V1,"value")]
  names(values) <- .GetFields(conf$V1,"value")
  connector.package <- conf$V2[conf$V1=="connector.package"]
  
  period.freq <- as.numeric(conf$V2[.GetFieldsId(conf$V1,"period.freq")])
  period.start <- as.numeric(strsplit(conf$V2[.GetFieldsId(conf$V1,"period.start")],"-")[[1]] )
  period.end <- as.numeric(strsplit(conf$V2[.GetFieldsId(conf$V1,"period.end")],"-")[[1]] )
  
  conf = conf[ .GetFieldsId(conf$V1,"eval.param"),"V2",drop=FALSE] 
  
  CONFIG=list(project.name = project.name,
    connector.package=connector.package,
    eval.package=eval.package,
    keys = keys, 
    values = values ,
    connector.package = connector.package,
    period.freq = period.freq,
    period.start = period.start,
    period.end = period.end,
    save=save)
  for (i in 1:nrow(conf))
    eval(parse(text=paste("CONFIG$param$",conf[i,]),))
  
  source(CONFIG$connector.package)		
  source(CONFIG$eval.package)
  
  CONFIG
}

GetItemsList <- function(project.path) {
  items.rdata <- paste( project.path, "items-list.Rdata", sep="/")
  load(items.rdata)
  Items
}

GetItemData <- function(project.path, keys) {
  load( paste(.GetItemPath(keys,project.path), "item.Rdata", sep="/"))
  item_data
}


BuildParamString <- function(param) {
  param <- lapply(param,function(p){if((length(p)==1)&(is.character(p))) p=paste("'",p,"'",sep="") else p })
  param <- param[names(param)!=""]
  gsub(" ","",gsub("\"","'",paste(names(param),param,sep="=",collapse=",")))
}

GetStrHTMLformEvalItem <- function(project.path, item.path, value, param) {
  
  param.string <- BuildParamString(param)
  paste(
        "<h3>Run the engine</h3>
                <form action=\"/strategico/eval_item.php\" method=\"post\" id=\"eval\"> 
            Params:
                          <input type=\"text\" name=\"params\" id=\"params\" size=\"160\" value=\"",param.string,"\" />
              <input type=\"hidden\" name=\"project_path\" value=\"",project.path,"\" />  
              <input type=\"hidden\" name=\"item_folder\" value=\"",item.path,"\" /> 
              <input type=\"hidden\" name=\"values\" value=\"",value,"\" /> <br />
              <input type=\"submit\" name=\"submit\" value=\"Run\" />                     
         </form>",sep="")
}

EvalItem <- function(project.path, keys=NULL, item.path=NULL, values = NULL, param=NULL) {

  if(!is.null(item.path)) keys=strsplit(item.path,"/")[[1]]

  item.data <- GetItemData(project.path, keys)
  EvalItemData(project.path, keys=keys, item.data=item.data, values = values, param=param)
}

EvalItemData <- function(project.path, keys=NULL, item.data, values = NULL, param=NULL) {
  if(!exists("CONFIG")) assign("CONFIG", GetProjectConfig(paste(project.path, "project.config", sep="/")), envir = .GlobalEnv)

  if(!is.null(keys)) print( paste(" Loading item: ", .GetItemName(keys) , sep=""))
  print( paste("  Time series: length=", nrow(item.data)))
  for (i in 1:length(values)) {
    value <- values[i]
    print( paste(" Evaluating ", value,": ", CONFIG$values[value], sep=""))  
    directory = .GetItemPath(keys,project.path,paste("report-",CONFIG$values[value], sep = ""))
    dir.create(directory, showWarnings = FALSE, recursive = TRUE)

    prediction = EvalItemDataByValue(project.path, keys, item.data, value=value, output.path=directory, param=param)
    print(t(prediction))
  }
}

EvalItemsFromDB <- function(project.name, value, verbose=FALSE) {
  tablename = GetSummaryDBTable(project.name, value)
  channel <- odbcConnect(STRATEGICO$db.out.name, STRATEGICO$db.out.user, STRATEGICO$db.out.pass, believeNRows=FALSE)

  statement <- paste("select * from ", tablename, " where Run=1", sep="")
  print(statement)
  items <- sqlQuery(channel, statement)
  odbcClose(channel)
  summary(items)
   idparam = which(names(items)=="Parameters")
  idKEYs = grep("KEY",names(items))

  for( i in 1:dim(items)[2]) {
        print(items[i,idKEYs]);
        if( (all(is.na(items[i,idKEYs]))) | (!all(sapply( items[i,idKEYs][!is.na(items[i,idKEYs])], is.character )) ))
                return(NA)
        else { print(items[i,idparam]);EvalItem(project.path, keys=items[i,idKEYs][!is.na(items[i,idKEYs])], values = value, param=eval(parse(text=paste("list(", gsub("Parameters='","", items[i,idparam]),")",sep="")))  )
}  }
}


ImportItemsData <- function(project.path) {
  if(!exists("CONFIG")) assign("CONFIG", GetProjectConfig(paste(project.path, "project.config", sep="/")), envir = .GlobalEnv)
  connector.importItemsData(project.path)
}

GetSummaryDBTable <- function(project.name, value) {
  paste(project.name, value, "summary", sep="_")
}

##trova un pattern in una lista di stringhe. utile per es per individuare le key e i value
##restituisce la stringa
.GetFields <- function(fields,pattern) {
  grep(paste("^",toupper(pattern),"[:digit:]*",sep=""), toupper(fields),value=TRUE)
}

##restituisce l'id
.GetFieldsId <- function(fields,pattern) {
  grep(paste("^",toupper(pattern),"[:digit:]*",sep=""), toupper(fields))
}


.SafeName <- function(String) {
  gsub("[ '/\"\\:-<>]+", "_", String)
}

.GetItemName <- function( keys ) { 
  new_keys <- sapply( keys, .SafeName)
  paste( new_keys, collapse="-")
}

.GetItemPath <- function( keys ,project.path=NULL,extra=NULL) {  
  new_keys <- sapply( keys[!is.na(keys)], .SafeName)
  subpath <- paste( new_keys, collapse="/")
  if (!is.null(project.path)) subpath <- paste(project.path, subpath, sep="/")
  if (!is.null(extra))  subpath <- paste(subpath,.SafeName(extra), sep="/")
  subpath
}

.StatsRecords <- function(filename, records, key, title="item records", top=25) {
  library(lattice)
  bitmap(filename)
  ## retriving top values
  ##data <- rev(sort(table(records[key])))
  data <-table(records[key])
  ##limit <- min(top, length(data))
  ##data <- data[1:limit]
  ## sorting by names
  ## data <- data[order(names(data))],
  img <- barchart(data,
                  scales = list(x=list(rot=90)),
                  horizontal=FALSE,
                  main=title,
                  col="green"
                  )
  print(img)
  dev.off()
}



###non sono riuscito a trovare .incSampleTime(
## immagino dovrebbe fare ciÃ² che segue
.incSampleTime <- function(now, period.freq = 2, increment = 1) {
  if (now[2] + increment - 1 <= period.freq - 1) 
    now[2] = now[2] + increment
  else now = c(now[1] + (now[2] - 1 + increment)%/%period.freq, 
         ((now[2] + increment - 1)%%period.freq) + 1)
  now
}

BuildPeriodRange <- function(period.start, period.freq, n) {
  sapply (0:(n-1), function(i) paste(.incSampleTime(now=period.start, period.freq = period.freq, increment = i),collapse="-"))
}
                            
.UpdateItemsDataRecursively <- function(project.path, data, keys, values=NULL, csv=FALSE, stats=FALSE) {
  if (is.null(values))
    folder <- project.path
  else
    folder <- paste(project.path, paste(values,collapse="/"), sep="/")

  print(folder)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  ##print( paste("Folder=", folder, "Key=", key) )
  vals.names <- .GetFields(names(data),"value")
  temp=by(data[,vals.names,drop=FALSE],data$PERIOD, function(x) apply(x,2, sum, na.rm=TRUE))
  item_data <- as.data.frame(t(matrix(unlist(temp),nrow=length(vals.names))))
  rownames(item_data) <- names(temp)[!sapply(temp,is.null)]
  colnames(item_data) <- vals.names
  save(item_data, file= paste(folder, "item.Rdata", sep="/"))
  
  if (csv)
    write.csv(item_data,
              file= paste(folder, "item.csv", sep="/"),
              row.names = FALSE
              )
  if (stats)
    .StatsRecords(
                   paste(folder, "item.png", sep="/"),
                   item_data,
                   key)
  
  if (length(keys) > 0) {
    key <- keys[1]
    newKeys <- keys[-1]
    keyValues <- levels(factor(data[,key]))   ###CHANGED
    for (keyValue in keyValues) {
      ##	   print(keyValue)
      newValues = values
      newValues[key] = (keyValue)
      ##	    print( values[1,])
      
      newData <- data[data[,key]==keyValue,]
      
      .UpdateItemsDataRecursively(project.path=project.path, data=newData, keys=newKeys, values=newValues)
    }
  }
}


###########################aggiornamento dati - crea items.Rdata e item.RData
UpdateItemsData <- function(project.path, projectData, csv=FALSE) {

  outfile <- paste(project.path, "/items.Rdata", sep="") 
  save( projectData, file=outfile)
  ## estrai/filtra la lista degli item e li salva nel file items-list.Rdata

  key_fields <- .GetFields( colnames(projectData) ,"key" )
  
  projectData$PERIOD <- factor(projectData$PERIOD)
  for (i in key_fields){
    projectData[,i] <- factor(projectData[,i])
    levels(projectData[,i]) <- .SafeName(levels(projectData[,i]))
  }
  
  leaves <- unique(subset(projectData, select=key_fields) )
  outfile <- paste(project.path, "/items-list.Rdata", sep="") 
  
  Items=leaves
  for (i in (ncol(leaves)):2){
    leaves[,i]=NA
    leaves= unique(leaves)
    Items=rbind(Items,unique(leaves))
  }
  save( Items, file=outfile)

  if (csv)
    write.csv(Items,
              file= paste(project.path, "/items.csv", sep=""),
              row.names = FALSE
              )	
  print(key_fields)			
  .UpdateItemsDataRecursively(project.path, projectData, keys=key_fields, values=NULL )
  
} # end function


BuildSQLstmtDeleteRecordsWithKeys <- function(tablename, key_names, key_values) {
  quoted_keys <- gsub("^(.*)$", "'\\1'", key_values)
  where_opt <- paste(key_names, quoted_keys, sep="=", collapse=" and ")

  delete_sql <- "delete from __TABLE__  where __WHERE_OPT__"
  delete_sql <- gsub("__TABLE__", tablename, delete_sql)
  delete_sql <- gsub("__WHERE_OPT__", where_opt, delete_sql)

}

ExportDataToDB <- function(data, tablename, key_values, verbose=FALSE) {
  channel <- odbcConnect(STRATEGICO$db.out.name, STRATEGICO$db.out.user, STRATEGICO$db.out.pass, believeNRows=FALSE)

  #key_values <- unlist(data[1,1:length(CONFIG$keys)])
  key_names <- names(CONFIG$keys)
  delete_sql <- BuildSQLstmtDeleteRecordsWithKeys(tablename, key_names, key_values)
  if(!is.null(delete_sql)) {
    #print(delete_sql)
    sqlQuery(channel, delete_sql)
  }
  sqlSave(channel, data, tablename=tablename, rownames=FALSE, append=TRUE, verbose=verbose)
  odbcClose(channel)
}

##input  da db. 
ImportItemsDataFromDB <- function(project.path, DB, DBUSER, DBPWD, sql_statement ) {
  
  channel <- odbcConnect(DB, DBUSER, DBPWD)
  result <- sqlQuery(channel, sql_statement)
  odbcClose(channel)

  UpdateItemsData(project.path, result)
}

##input da da csv. 
ImportItemsDataFromCSV <- function(project.path, filename=NULL, KEY=c("KEY1","KEY2"), timesKeys=c("YEAR","SEMESTER"), VALUE=c("CORP")){ 

  ##restituisce una list (itemList) con una ts per ogni elemento. 
  ##names(itemList) è una parola composta dai valori assunti nei campi indicati da keys. separatore "[" 
  ##torna utile in seguito, nelle creazioni degli output dell'analisi
  
  if (is.null(filename)) filename=file.choose()
  data=read.csv(filename,sep=",") 

  # names.data <- names(data)
  # names(names.data) <- names(data)

  # names.data[KEY]=paste(KEY,1:length(KEY),sep="")
  # names.data[VALUE]=paste(VALUE,1:length(VALUE),sep="")

  #names(data) = names.data
  
  if(length(timesKeys)>1) data$PERIOD=paste(data[,timesKeys[1]],data[,timesKeys[2]],sep="-")
  else data$PERIOD=data[,timesKeys]

  UpdateItemsData(project.path, data[,c(KEY,"PERIOD",VALUE)])
}

