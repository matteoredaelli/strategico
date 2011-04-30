#!/usr/bin/env Rscript
## This program is fre esoftware: you can redistribute it and/or modify
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

library("futile.logger")
library("xtable")

Quit <- function (msg="", status=-1) {
  print(msg)
  print("Bye!")
  q(status=status)
}

FileExistsOrQuit <- function (filename, msg="", status=10){
  if (!file.exists(filename)) {
    print( paste("File", filename, "not found!"))
    Quit(msg=msg, status=status)   
  }
}

GetStrategicoHome <- function() {
  strategico.path <-as.character(Sys.getenv("STRATEGICO_HOME"))
  if (strategico.path == "") {
    print("Environment STRATEGICO_HOME not set, Bye!")
    q(status=2)
  }
  strategico.path
}

MySource <- function(filename) {
  strategico.path <- GetStrategicoHome()
  fullname = file.path(strategico.path, filename)
  FileExistsOrQuit(fullname)
  source(fullname)
}

MySource("strategico.config")

config_logger(threshold = strategico.config$logger.threshold)
logger <- getLogger()

MySource("strategico_db.R")

AggregateItemData <- function(data, value) {
  options(na.action="na.omit")

  d2 <- aggregate(data[[value]],  by=list(data[["PERIOD"]]),  FUN=sum, na.rm=TRUE)
  names(d2) <- c('PERIOD', value)
  rownames(d2) <- d2$PERIOD
  d2$PERIOD <- NULL
  d2
}

#UNUSED
BuildFullKey <- function(keys, config.keys, fill.with="") {
  append(keys, rep(fill.with, length(config.keys) - length(keys)))
}

## TODO should manage NA (and NULL) as alias of  ''
BuildKeyNames <- function(key.values, na.rm=FALSE) {
  idx = if (na.rm)
    grep('^$', key.values, invert=TRUE)
  else
    seq(1,length(key.values))
  
  paste("KEY", idx, sep="")
}

BuildFilterWithKeys <- function(key.values, sep="=", collapse=",", na.rm=FALSE) {
  ## a filter can be like "KEY1=='IT' & KEY2=='101'
  ## see runit test file for more samples
  
  key.values[is.na(key.values)] = ""
  key.names <- BuildKeyNames(key.values, na.rm=na.rm)
  if (na.rm)
    key.values <- key.values[ key.values != "" ]
  quoted.keys <- gsub("^(.*)$", "'\\1'", key.values)
  paste(key.names, quoted.keys, sep=sep, collapse=collapse)
}

BuildParamString <- function(param) {
  param <- lapply(param,function(p){if((length(p)==1)&(is.character(p))) p=paste("'",p,"'",sep="") else p })
  param <- param[names(param)!=""]
  gsub(" ","",gsub("\"","'",paste(names(param),param,sep="=",collapse=";")))
}

BuildPeriodRange <- function(period.start, period.freq, n, shift=0) {
  sapply ((0+shift):(n+shift-1), function(i) paste(.incSampleTime(now=period.start, period.freq = period.freq, increment = i),collapse="-"))
}

EvalItems <- function(project.name, id.min, id.max, keys=NULL, values, param=NULL, project.config=NULL, db.channel) {
  if (is.null(project.config)) {
    project.config <- GetProjectConfig(project.name=project.name)
  }
  
  for (id in as.integer(id.min):as.integer(id.max)) {
    EvalItem(project.name=project.name, id=id, keys=keys, values=values, param=param, project.config=project.config, db.channel=db.channel)
  }
}

EvalItem <- function(project.name, id=NULL, keys=NULL, values, param=NULL, project.config, db.channel) {
  for (i in 1:length(values)) {
    value <- values[i]
    EvalItemData(project.name=project.name, id=id, keys=keys, value=value, param=param,
                 project.config=project.config,db.channel=db.channel)
  }
}

EvalItemData <- function(project.name, id=NULL, keys=NULL, item.data=NULL, value,
                         param=NULL, project.config, db.channel) {
  logger(INFO, "++++++++++++++++++++++++EvalItemData ++++++++++++++++++++++++")
  logger(INFO, paste("Project=", project.name, " Loading item ID=", id,
                     " KEYS=", paste(keys,collapse=","), " ",
                     value, "=", project.config$values[value],
                     sep=""))
  
  if (is.null(item.data))
    item.data <- GetItemData(project.name=project.name, id=id, keys=keys, value=value)
  
  logger(INFO, paste("TS length=", nrow(item.data)))
  print( t(item.data))
  
  logger(INFO, paste("period.start=", paste(project.config$period.start, collapse="-"),
                     " period.freq=", project.config$period.freq,
                     " period.end=", paste(project.config$period.end, collapse="-"),
                     sep=""))
  
  if (is.null(id)) {
    logger(INFO, "ID is null, assigning a new value")
    id <- GetNewID()
  }
  
  param <- MergeParamWithDefault(project.config=project.config, param=param)

  logger(DEBUG, paste("Param= ",BuildParamString(param)))
  
  directory = GetItemPath(project.name, id, value)
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  EvalFunction <- paste(project.config$eval.function,".EvalItemDataByValue(project.name=project.name, id=id, item.data=item.data,
    value=value, output.path=directory, param=param, project.config=project.config, db.channel=db.channel)", sep="")

  prediction <- eval(parse(text=EvalFunction))
  logger(INFO, "RESULTS:")
  print(t(prediction))
  t(prediction)
}

EvalParamString <- function(param.string) {
  if (is.character(param.string)) {
    param.string <- gsub(";",",", param.string)
    param <- eval(parse(text=paste("list(",param.string,")")))
  } else {
    param <- list()
  }
  param
}

EvalTS <- function(project.name, id=NULL, ts.values, ts.periods, period.start, period.freq,
                   calculate.period.end=TRUE, param=NULL, project.config, value="V1", db.channel ) {
  item.data <- cbind(ts.values)
  rownames(item.data) <-ts.periods
  colnames(item.data) <- c(value)

  project.config$period.start <-  period.start
  
  project.config$period.freq = period.freq
  
  if (calculate.period.end) {  
    period.end.string <- rownames(item.data)[ length(ts.periods)]
    period.end <- unlist(lapply(strsplit(period.end.string, "-"), as.numeric))
    project.config$period.end = period.end
  } # otherwise the project config value will be used
  
  EvalItemData(project.name=project.name, id=id, item.data=item.data, value=value,
               param=param, project.config=project.config, db.channel=db.channel)
}

EvalTSString <- function(project.name, id=NULL, ts.string,
                         ts.periods.string=NULL, period.start.string, period.freq,
                         calculate.period.end=TRUE, param=NULL, project.config=NULL, db.channel) {
  if (is.null(project.config)) {
    project.config <- GetProjectConfig(project.name=project.name)
  }
  ts.values <- unlist(lapply(strsplit(ts.string,","), as.numeric))

  period.start <- PeriodStringToVector(period.start.string)
  period.freq <- as.integer(period.freq)

  ts.periods <- BuildPeriodRange(period.start, period.freq, length(ts.values))
  
  if(is.character(ts.periods.string)) {
    ts.periods.tmp <- unlist(lapply(strsplit(ts.periods.string, ","), as.character))
    if (length(ts.periods.tmp) == length(ts.values))
      ts.periods = ts.periods.tmp
    else
      logger(WARN, "Skipping ts.periods string, not matching length with ts string")
  }
  

  
  EvalTS(project.name, id=id, ts.values=ts.values, ts.periods=ts.periods, period.start=period.start,
         period.freq=period.freq, calculate.period.end=calculate.period.end, param=param,
         project.config=project.config, db.channel=db.channel)
}

## trova un pattern in una lista di stringhe.
## utile per es per individuare le key e i value
## restituisce la stringa
.GetFields <- function(fields,pattern) {
  grep(paste("^",toupper(pattern),"[:digit:]*",sep=""), toupper(fields),value=TRUE)
}

## restituisce l'id
.GetFieldsId <- function(fields,pattern) {
  grep(paste("^",toupper(pattern),"[:digit:]*",sep=""), toupper(fields))
}

GetItemData <- function(project.name=NULL, project.data=NULL, id=NULL, keys=NULL, value, keys.na.rm=TRUE) {
  if (is.null(value)) {
    logger(INFO, "Missing V parameter in GetItemData")
  }
  if (is.null(project.data))
    project.data <- GetProjectData(project.name=project.name)

  if (is.null(keys)) 
    filtered.data <- SubsetByID(data=project.data, id=id)
  else
    filtered.data <- SubsetByKeys(data=project.data, keys=keys, keys.na.rm=keys.na.rm)

  if (nrow(filtered.data) > 0)
    result <- AggregateItemData(filtered.data, value=value)
  else {
    logger(INFO, "No rows: cannot aggregate data")
    result = filtered.data
  }
  result 
}

 
GetItemID <- function(keys, project.name=NULL, project.items=NULL) {
  if (is.null(project.items))
    project.items <- GetProjectItems(project.name=project.name)
  
  records <- SubsetByKeys(data=project.items, keys=keys, keys.na.rm=FALSE)
  tot <- nrow(records)
  if (tot == 0) {
    logger(WARN, paste("No id found for KEYS", keys, sep=' ', collapse=','))
    result = NA
  } else {
    result = as.integer(records[1,]["id"])
    if (tot > 1) {
      logger(WARN,  paste("To many IDs for key", keys, sep=' ', collapse=','))
    }
  }
  result
}

GetItemKeys <- function(id, project.name=NULL, project.items=NULL) {
  if (is.null(project.items))
    project.items <- GetProjectItems(project.name=project.name)
  
  cmd <- "ds <- subset(project.items, id==__ID__, select=c(-id))"
  cmd <- gsub("__ID__", id, cmd)
  eval(parse(text = cmd))
  if (nrow(ds) >0)
    result <- as.matrix(ds)[1, ]
  else {
    logger(WARN, paste("No keys found for ID=", id))
    result <- NULL
  }
  result
}

GetItemRelativePath <- function(id, value=NULL) { 
  path <- file.path(as.integer(id / 500), id)
  if( !is.null(value))
    path <- file.path(path, value)
  path
}

GetItemPath <- function(project.name, id, value=NULL) {
  project.path <- GetProjectPath(project.name)
  relative.path <- GetItemRelativePath(id, value)
  paste(project.path, relative.path, sep="/")
}

GetItemUrl <- function(project.name, id, value=NULL) {
  project.url <- GetProjectUrl(project.name)
  relative.path <- GetItemRelativePath(id, value)
  paste(project.url, relative.path, sep="/")
}

GetItemResultsDB <- function(project.name, value, id) {
  tablename <- GetDBTableNameItemResults(project.name, value=value)
  GetItemRecordsFromDB(project.name, id, tablename)
}

GetKeyNames <- function(keys=NULL, project.name=NULL, project.config=NULL) {
  if (is.null(keys)) {
    if (is.null(project.config))
      project.config <- GetProjectConfig(project.name)
   
    keys <- project.config$keys
  }
  paste("KEY", 1:length(keys), sep="")
}

GetNewID <- function(from=strategico.config$id.dummies.from, to=strategico.config$id.dummies.to) {
  sample(from:to,1)
}

GetProjectItems <- function(project.name) {
  project.path <- GetProjectPath(project.name)
  filename <- file.path(project.path, "project_items.Rdata")
  FileExistsOrQuit(filename)
  load(filename)
  project.items
}

GetProjectData <- function(project.name) {
  project.path <- GetProjectPath(project.name)
  filename <- file.path(project.path, "project_data.Rdata")
  FileExistsOrQuit(filename)
  load(filename)
  project.data
}

GetProjectConfig <- function(project.name) {
  project.path <- GetProjectPath(project.name)
  filename <- file.path(project.path, "project.config")
  
  FileExistsOrQuit(filename)
  ## sourcing priect.config file
  source(filename)

  eval.file <- paste("eval_", project.config$eval.function, ".R", sep="")
  MySource(eval.file)
 
  ##append(project.config, strategico.config)
  project.config
}

GetProjectPath <- function(project.name, projects.home = strategico.config$projects.home) {
  file.path(projects.home, project.name)
}

GetProjectUrl <- function(project.name, projects.url = strategico.config$projects.url) {
  paste(projects.url, project.name, sep="/")
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

GetUniqueKeyValues <- function(project.name=NULL, project.items=NULL, project.config) {
  if (is.null(project.items))
    project.items <- GetProjectItems(project.name=project.name)

  keys <- paste("KEY", 1:length(project.config$keys), sep="")
  sapply(keys, function(x) unique(project.items[[x]]))
}

GetValueNames <- function(values=NULL, project.name=NULL, project.config=NULL) {
  if (is.null(values)) {
    if (is.null(project.config))
      project.config <- GetProjectConfig(project.name)
    values <- project.config$values
  }
  paste("V", 1:length(values), sep="")
}

ImportProjectData <- function(project.name, project.config=NULL, db.channel) {
  if (is.null(project.config))
    project.config <- GetProjectConfig(project.name=project.name)

   
  project.R <- paste("project_", project.name, ".R", sep="")
  MySource(project.R)
  
  cmd <- paste(project.name,".importItemsData(project.name=project.name)", sep="")
  result <- eval(parse(text = cmd))
  UpdateItemsData(project.name, result, db.channel)
}

##input da da csv. 
ImportProjectDataFromCSV <- function(project.name, filename=NULL, KEY=c("KEY1","KEY2"),
                                     timesKeys=c("YEAR","SEMESTER"), V=c("CORP")){ 

  ##restituisce una list (itemList) con una ts per ogni elemento. 
  ##names(itemList) è una parola composta dai valori assunti nei campi indicati da keys. separatore "[" 
  ##torna utile in seguito, nelle creazioni degli output dell'analisi
  
  if (is.null(filename)) filename=file.choose()
  data=read.csv(filename,sep=",") 
  
  if(length(timesKeys)>1) data$PERIOD=paste(data[,timesKeys[1]],data[,timesKeys[2]],sep="-")
  else data$PERIOD=data[,timesKeys]

  result <- data[,c(KEY,"PERIOD",V)]
}

.incSampleTime <- function(now, period.freq = 2, increment = 1) {
  if (now[2] + increment - 1 <= period.freq - 1) 
    now[2] = now[2] + increment
  else now = c(now[1] + (now[2] - 1 + increment)%/%period.freq, 
         ((now[2] + increment - 1)%%period.freq) + 1)
  now
}

MergeParamWithDefault <- function(project.name=NULL, project.config=NULL, param) {
  if (is.null(project.config))
    project.config <- GetProjectConfig(project.name=project.name)
  
  c(param,project.config$param[setdiff(names(project.config$param),names(param))])
}

PeriodStringToVector <- function (period.string) {
  unlist(lapply(strsplit(period.string, "-"), as.numeric))
}

SubsetByKeys <- function(data, keys, keys.na.rm=TRUE) {
  filter <- BuildFilterWithKeys(keys, sep="==", collapse=" & ", na.rm=keys.na.rm)
  logger(DEBUG, filter)
  cmd <- "records <- subset(data, __FILTER__)"
  cmd <- gsub("__FILTER__", filter, cmd)
  eval(parse(text = cmd))
  records
}

SubsetByID <- function(data, id) {
  cmd <- "subset(data, id==__ID__)"
  cmd <- gsub("__ID__", id, cmd)
  eval(parse(text = cmd))
}

.UpdateItemsDataRecursively.ununsed <- function(project.path, data, keys, values=NULL, stats=FALSE) {
  if (is.null(values))
    folder <- project.path
  else
    folder <- paste(project.path, paste(values,collapse="/"), sep="/")

  logger(WARN, folder)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  vals.names <- .GetFields(names(data),"value")
  temp=by(data[,vals.names,drop=FALSE],data$PERIOD, function(x) apply(x,2, sum, na.rm=TRUE))
  item_data <- as.data.frame(t(matrix(unlist(temp),nrow=length(vals.names))))
  rownames(item_data) <- names(temp)[!sapply(temp,is.null)]
  colnames(item_data) <- vals.names
  save(item_data, file= paste(folder, "item.Rdata", sep="/"))
  
  if("items_csv"%in%project.config$save)
    write.csv(item_data,
              file= paste(folder, "item.csv", sep="/"),
              row.names = FALSE
              )
  
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


## creates item.Rdata e item-list
UpdateItemsData <- function(project.name, project.data, db.channel) {
  project.path <- GetProjectPath(project.name)
  project.config <- GetProjectConfig(project.name=project.name)
  
  ## estrai/filtra la lista degli item e li salva nel file items-list.Rdata

  key_fields <- .GetFields( colnames(project.data) ,"key" )
  
  project.data$PERIOD <- factor(project.data$PERIOD)
  for (i in key_fields){
    project.data[,i] <- factor(project.data[,i])
    levels(project.data[,i]) <- levels(project.data[,i])
  }
  
  leaves <- unique(subset(project.data, select=key_fields) )
  outfile <- paste(project.path, "project_items.Rdata", sep="/") 
  
  project.items=leaves
  for (i in (ncol(leaves)):2){
    leaves[,i]=""
    leaves= unique(leaves)
    project.items=rbind(project.items,unique(leaves))
  }

  # adding ID column
  project.items <- cbind(id=1:nrow(project.items), project.items)
  save( project.items, file=outfile)

  if("items_csv"%in%project.config$save)
    write.csv(project.items,
              file= paste(project.path, "/project_items.csv", sep=""),
              row.names = FALSE
              )
  if("items_db"%in%project.config$save) {

    tablename = GetDBTableNameProjectItems(project.config$project.name)
    ## preparing data for prymary key in DB  (id must be the rownames)
    project.items.orig <- project.items
    rownames(project.items) <- project.items$id
    project.items$id <- NULL
    
    ExportDataToDB(project.items, tablename, id=NULL, rownames="id", addPK=TRUE, db.channel=db.channel)
    
    project.items <- project.items.orig
  }

  ## Putting item ID inside project.data
  project.data <- merge(project.items, project.data)
  
  outfile <- paste(project.path, "project_data.Rdata", sep="/") 
  save(project.data, file=outfile)
  #print(key_fields)			
  #.UpdateItemsDataRecursively(project.path, project.data, keys=key_fields, values=NULL )
  
} # end function

Version <- function() {"
Strategico - Copyright(c) 2010, 2011
Release 1.0.0
License: GPL V3*

Contributors:
  Livio Finos
  Matteo Redaelli
  Marco Rinaldo
"
}

