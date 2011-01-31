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
  items.rdata <- paste( project.path, "items.Rdata", sep="/")
  load(items.rdata)
  Items
}

GetItemData <- function(project.path, keys) {
  load( paste(.GetItemPath(keys,project.path), "item.Rdata", sep="/"))
  item_data
}

EvalItem <- function(project.path, keys=NULL, pathToItem=NULL, values = NULL, param=NULL) {
  if(!exists("CONFIG")) assign("CONFIG", GetProjectConfig(paste(project.path, "project.config", sep="/")), envir = .GlobalEnv)

  if(!is.null(pathToItem)) keys=strsplit(pathToItem,"/")[[1]]

  item.data <- GetItemData(project.path, keys)

  if (is.null(values)) values <- names(CONFIG$values)

  if(!is.null(keys)) print( paste(" Loading item: ", .GetItemName(keys) , sep=""))
  for (i in 1:length(values)) {
    print( paste(" Evaluating ", values[i],": ", CONFIG$values[values[i]], sep=""))  
    prediction = EvalItemByValue(project.path, keys, item.data, value=values[i],param=param)
    print(t(prediction))
  }
}

ImportItemsData <- function(project.path) {
  if(!exists("CONFIG")) assign("CONFIG", GetProjectConfig(paste(project.path, "project.config", sep="/")), envir = .GlobalEnv)
  connector.importItemsData(project.path)
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

.UpdateItemsDataRecursively <- function(project.path, data, keys, values=NULL, csv=FALSE, stats=FALSE) {
  if (is.null(values))
    folder <- project.path
  else
    folder <- paste(project.path, paste(values,collapse="/"), sep="/")

  print(folder)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  ##print( paste("Folder=", folder, "Key=", key) )
  vals.names <- .GetFields(names(data),"value")
  temp=by(data[,vals.names],data$PERIOD, function(x) apply(x,2, sum, na.rm=TRUE))
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

  ## estrai/filtra la lista degli item e li salva nel file items.Rdata

  key_fields <- .GetFields( colnames(projectData) ,"key" )
  
  projectData$PERIOD <- factor(projectData$PERIOD)
  for (i in key_fields){
    projectData[,i] <- factor(projectData[,i])
    levels(projectData[,i]) <- .SafeName(levels(projectData[,i]))
  }
  
  leaves <- unique(subset(projectData, select=key_fields) )
  outfile <- paste(project.path, "/items.Rdata", sep="") 
  
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


##input  da db. 
ImportItemsDataFromDB <- function(project.path, DB, DBUSER, DBPWD, sql_statement ) {
  
  library(RODBC)

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

  names.data <- names(data)
  names(names.data) <- names(data)

  names.data[KEY]=paste(KEY,1:length(KEY),sep="")
  names.data[VALUE]=paste(VALUE,1:length(VALUE),sep="")

  names(data) = names.data
  data$PERIOD=.GetItemName(data[,timesKeys])

  UpdateItemsData(project.path, data[,c(KEY,"PERIOD",VALUE)])
}
