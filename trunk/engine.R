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
library("futile.logger")

source("strategico.config")

config_logger(threshold = STRATEGICO$logger.threshold)
logger <- getLogger()
              
BuildFullKey <- function(keys, config.keys, fill.with="") {
  append(keys, rep(fill.with, length(config.keys) - length(keys)))
}

BuildKeyNames <- function(key.values, na.rm=FALSE) {
  idx = if (na.rm)
    grep('^$', key.values, invert=TRUE)
  else
    seq(1,length(key.values))
  
  paste("KEY", idx, sep="")
}

BuildFilterWithKeys <- function(key.values, sep="=", collapse=",", na.rm=FALSE) {
  ## a filter can be like "KEY1=='IT' & KEY2=='101
  ## see runit test file for more samples
  
  key.values[is.na( key.values)] = ""
  key.names <- BuildKeyNames(key.values, na.rm=na.rm)
  if (na.rm)
    key.values <- key.values[ key.values != "" ]
  quoted.keys <- gsub("^(.*)$", "'\\1'", key.values)
  paste(key.names, quoted.keys, sep=sep, collapse=collapse)
}

BuildParamString <- function(param) {
  param <- lapply(param,function(p){if((length(p)==1)&(is.character(p))) p=paste("'",p,"'",sep="") else p })
  param <- param[names(param)!=""]
  gsub(" ","",gsub("\"","'",paste(names(param),param,sep="=",collapse=",")))
}

BuildPeriodRange <- function(period.start, period.freq, n, shift=0) {
  sapply ((0+shift):(n+shift-1), function(i) paste(.incSampleTime(now=period.start, period.freq = period.freq, increment = i),collapse="-"))
}
  
BuildSQLstmtDeleteRecordsWithKeys <- function(tablename, key_values, na.rm=FALSE) {
  where_opt <- BuildFilterWithKeys(key_values, sep="=", collapse=" and ", na.rm)

  delete_sql <- "delete from __TABLE__  where __WHERE_OPT__"
  delete_sql <- gsub("__TABLE__", tablename, delete_sql)
  
  gsub("__WHERE_OPT__", where_opt, delete_sql)
}

EvalItem <- function(project.path, keys=NULL, item.path=NULL, values = NULL, param=NULL, CONFIG) {
  for (i in 1:length(values)) {
    value <- values[i]
    EvalItemValue(project.path, keys=keys, item.path=item.path, value = value, param=param, CONFIG=CONFIG)
  }
}

EvalItemValue <- function(project.path, keys=NULL, item.path=NULL, value = "VALUE1", param=NULL, CONFIG) {
  if(!is.null(item.path)) keys=strsplit(item.path,"/")[[1]]

  item.data <- GetItemData(project.path, keys)
  EvalItemData(project.path, keys=keys, item.data=item.data, values = value, param=param, CONFIG=CONFIG)
}

EvalItemFromProjectData <- function(project.path, keys, value = "VALUE1", param=NULL, CONFIG) {
  p <- GetProjectData(project.path)
  item.data <- ExtractAndAggregateItemDataFromProjectData(p, keys, value)
  EvalItemData(project.path, keys=keys, item.data=item.data, values = value, param=param, CONFIG=CONFIG)
}

EvalItemData <- function(project.path, keys=NULL, item.data, values = NULL, param=NULL, CONFIG) {
  value = values
  if(!is.null(keys)) logger(INFO, paste("Loading item=", .GetItemName(keys) , sep=""))
  
  logger(INFO, paste("Evaluating ", value, "=", CONFIG$values[value], sep=""))
  logger(INFO, paste("TS length=", nrow(item.data)))
  logger(INFO, paste("period.start=", paste(CONFIG$period.start, collapse="-"),
               " period.freq=", CONFIG$period.freq,
               " period.end=", paste(CONFIG$period.end, collapse="-")))
  print( t(item.data))
  directory = .GetItemPath(keys,project.path,paste("report-",CONFIG$values[value], sep = ""))
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  prediction = EvalItemDataByValue(project.path, keys, item.data, value=value, output.path=directory, param=param, CONFIG=CONFIG)
  print(t(prediction))
  t(prediction)
}

EvalItemsFromDB <- function(project.name, value, verbose=FALSE, CONFIG) {
  tablename = GetSummaryDBTable(project.name, value)
  channel <- odbcConnect(STRATEGICO$db.out.name, STRATEGICO$db.out.user, STRATEGICO$db.out.pass, believeNRows=FALSE)

  statement <- paste("select * from ", tablename, " where Run=1", sep="")
  logger(WARN, statement)
  items <- sqlQuery(channel, statement)
  odbcClose(channel)
  summary(items)
  idparam = which(names(items)=="Parameters")
  idKEYs = grep("KEY",names(items))

  for( i in 1:dim(items)[2]) {
    logger(WARN, items[i,idKEYs]);
    if( (all(is.na(items[i,idKEYs]))) | (!all(sapply( items[i,idKEYs][!is.na(items[i,idKEYs])], is.character )) ))
      return(NA)
    else {
      logger(WARN, items[i,idparam])
      EvalItem(project.path, keys=items[i,idKEYs][!is.na(items[i,idKEYs])], CONFIG=CONFIG, values = value,
               param=eval(parse(text=paste("list(", gsub("Parameters='","", items[i,idparam]),")",sep="")))  )
    }
  }
}

EvalParamString <- function(param.string) {
  eval(parse(text=paste("param=list(",param.string,")")))
  param
}

EvalTS <- function(project.path, keys=NULL, ts.values, ts.periods, period.start, period.freq, calculate.period.end=TRUE, param=NULL, CONFIG) {
  item.data <- cbind(ts.values)
  
  rownames(item.data) <-ts.periods
  colnames(item.data) <- c("VALUE1")

  CONFIG$period.start <-  period.start
  
  CONFIG$period.freq = period.freq
  
  if (calculate.period.end) {  
    period.end.string <- rownames(item.data)[ length(ts.periods)]
    period.end <- unlist(lapply(strsplit(period.end.string, "-"), as.numeric))
    CONFIG$period.end = period.end
  } # otherwise the project config value will be used
  
  EvalItemData(project.path, keys=keys, item.data=item.data, values = "VALUE1", param=param, CONFIG=CONFIG)
}

EvalTSString <- function(project.path, keys=NULL, ts.string, ts.periods.string=NULL, period.start.string, period.freq, calculate.period.end=TRUE, param=NULL, CONFIG) {

  ts.values <- unlist(lapply(strsplit(ts.string,","), as.numeric))

  period.start <- PeriodStringToVector(period.start.string)
  period.freq <- as.integer(period.freq)
  
  if(is.null(ts.periods.string))
    ts.periods <- BuildPeriodRange(period.start, period.freq, length(ts.values))
  else
    ts.periods <- unlist(lapply(strsplit(ts.periods.string, ","), as.character))
  
  EvalTS(project.path, keys=keys, ts.values=ts.values, ts.periods=ts.periods, period.start=period.start,
         period.freq=period.freq, calculate.period.end=calculate.period.end, param=param, CONFIG=CONFIG)
}

ExportDataToDB <- function(data, tablename, key_values=NULL, verbose=FALSE, rownames=FALSE, append=TRUE) {
  channel <- odbcConnect(STRATEGICO$db.out.name, STRATEGICO$db.out.user, STRATEGICO$db.out.pass, believeNRows=FALSE)
  if(!is.null(key_values)) {
    
    delete_sql <- BuildSQLstmtDeleteRecordsWithKeys(tablename, key_values)
    if(!is.null(delete_sql)) {
      logger(DEBUG, delete_sql)
      sqlQuery(channel, delete_sql)
    }
  }
  
  sqlSave(channel, data, tablename=tablename, rownames=rownames, append=append, verbose=verbose)
  odbcClose(channel)
}

ExtractItemDataFromProjectData <- function(project.data, key.values, value="VALUE1") {
  filter <- BuildFilterWithKeys( key.values, sep="==", collapse=" & ", na.rm=TRUE)
  cmd <- "subset(project.data, __FILTER__, select=c('PERIOD','__VALUE__'))"
  cmd <- gsub("__FILTER__", filter, cmd)
  cmd <- gsub("__VALUE__", value, cmd)
  eval(parse(text = cmd))
}

ExtractAndAggregateItemDataFromProjectData <- function(project.data, key.values, value="VALUE1") {
  d1 <- ExtractItemDataFromProjectData(project.data, key.values, value)
  options(na.action="na.omit")
  #cmd <- "d2 <- aggregate(d1$__VALUE__, by=list(d1$PERIOD), FUN=sum, na.rm=TRUE)"
  #cmd <- gsub("__VALUE__", value, cmd)
  #eval(parse(text = cmd))

  d2 <- aggregate(d1[[value]],  by=list(d1[["PERIOD"]]),  FUN=sum, na.rm=TRUE)
  names(d2) <- c('PERIOD', value)
  
  ## change names
  #cmd <- "names(d2) <- c('PERIOD', '__VALUE__')"
  #cmd <- gsub("__VALUE__", value, cmd)
  #eval(parse(text = cmd))
  rownames(d2) <- d2$PERIOD
  d2$PERIOD <- NULL
  d2
}

GetDBTable <- function(project.name, value=NULL, name=NULL) {
  ext <- paste(c(value, name), collapse="_")
  paste(project.name, ext, sep="_")
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

GetItemsList <- function(project.path) {
  items.rdata <- paste( project.path, "items-list.Rdata", sep="/")
  load(items.rdata)
  project.items
}

GetItemData <- function(project.path, keys) {
  load( paste(.GetItemPath(keys,project.path), "item.Rdata", sep="/"))
  item_data
}

GetItemsListDB <- function(project.name, value, keys) {
  filter <- BuildFilterWithKeys( keys, sep="=", collapse=" and ", na.rm=FALSE)
  sql_statement <- paste("select * from", GetDBTable(project.name, "items", value=NULL), "where", filter, sep=" ")
  logger(WARN, sql_statement)
  RunSQLQueryDB(sql_statement)
}

GetItemDataDB <- function(project.name, value, keys) {
  filter <- BuildFilterWithKeys( keys, sep="=", collapse=" and ", na.rm=FALSE)
  sql_statement <- paste("select * from", GetDBTable(project.name, value=value), "where", filter, sep=" ")
  logger(WARN, sql_statement)
  RunSQLQueryDB(sql_statement)
}
GetItemDBSummary <- function(project.name, value, keys) {
  filter <- BuildFilterWithKeys( keys, sep="=", collapse=" and ", na.rm=FALSE)
  sql_statement <- paste("select * from", GetDBTable(project.name, name="summary", value=value), "where", filter, sep=" ")
  logger(WARN, sql_statement)
  RunSQLQueryDB(sql_statement)
}

GetProjectData <- function(project.path) {
  load( paste(project.path, "project.data.Rdata", sep="/"))
  project.data
}

GetProjectConfig <- function(project.config.fileName="project.config") {
  ## cerca il file nella cartella : getwd()
  conf=read.table(project.config.fileName, head=FALSE,sep=":",stringsAsFactors =FALSE,quote="\"")
  ## e assegnazione dei valori indicati dal file ai parametri
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


ImportItemsData <- function(project.path) {
  if(!exists("CONFIG")) assign("CONFIG", GetProjectConfig(paste(project.path, "project.config", sep="/")), envir = .GlobalEnv)
  connector.importItemsData(project.path)
}

##input  da db. 
ImportItemsDataFromDB <- function(project.path, DB, DBUSER, DBPWD, sql_statement ) {
  result <- RunSQLQueryDB(sql_statement)
  UpdateItemsData(project.path, result)
}

##input da da csv. 
ImportItemsDataFromCSV <- function(project.path, filename=NULL, KEY=c("KEY1","KEY2"), timesKeys=c("YEAR","SEMESTER"), VALUE=c("CORP")){ 

  ##restituisce una list (itemList) con una ts per ogni elemento. 
  ##names(itemList) è una parola composta dai valori assunti nei campi indicati da keys. separatore "[" 
  ##torna utile in seguito, nelle creazioni degli output dell'analisi
  
  if (is.null(filename)) filename=file.choose()
  data=read.csv(filename,sep=",") 
  
  if(length(timesKeys)>1) data$PERIOD=paste(data[,timesKeys[1]],data[,timesKeys[2]],sep="-")
  else data$PERIOD=data[,timesKeys]

  UpdateItemsData(project.path, data[,c(KEY,"PERIOD",VALUE)])
}

.incSampleTime <- function(now, period.freq = 2, increment = 1) {
  if (now[2] + increment - 1 <= period.freq - 1) 
    now[2] = now[2] + increment
  else now = c(now[1] + (now[2] - 1 + increment)%/%period.freq, 
         ((now[2] + increment - 1)%%period.freq) + 1)
  now
}

PeriodStringToVector <- function (period.string) {
  unlist(lapply(strsplit(period.string, "-"), as.numeric))
}

RunSQLQueryDB <- function(sql_statement ) {
  channel <- odbcConnect(STRATEGICO$db.out.name, STRATEGICO$db.out.user, STRATEGICO$db.out.pass, believeNRows=FALSE)
  result <- sqlQuery(channel, sql_statement)
  odbcClose(channel)

  result
}

.SafeName <- function(String) {
  gsub("[ '/\"\\:-<>]+", "_", String)
}

.UpdateItemsDataRecursively <- function(project.path, data, keys, values=NULL, stats=FALSE) {
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
  
  if("items_csv"%in%CONFIG$save)
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
UpdateItemsData <- function(project.path, project.data) {
  outfile <- paste(project.path, "/project.data.Rdata", sep="") 
  save( project.data, file=outfile)
  ## estrai/filtra la lista degli item e li salva nel file items-list.Rdata

  key_fields <- .GetFields( colnames(project.data) ,"key" )
  
  project.data$PERIOD <- factor(project.data$PERIOD)
  for (i in key_fields){
    project.data[,i] <- factor(project.data[,i])
    levels(project.data[,i]) <- .SafeName(levels(project.data[,i]))
  }
  
  leaves <- unique(subset(project.data, select=key_fields) )
  outfile <- paste(project.path, "/items.Rdata", sep="") 
  
  project.items=leaves
  for (i in (ncol(leaves)):2){
    leaves[,i]=NA
    leaves= unique(leaves)
    project.items=rbind(project.items,unique(leaves))
  }

  # adding ID column
  project.items <- cbind(id=1:nrow(project.items), project.items)
  save( project.items, file=outfile)

  if("items_csv"%in%CONFIG$save)
    write.csv(project.items,
              file= paste(project.path, "/items.csv", sep=""),
              row.names = FALSE
              )
  if("items_db"%in%CONFIG$save) {
    tablename = GetDBTable(CONFIG$project.name, value=NULL, name="items")
    ExportDataToDB(project.items, tablename, key_values=NULL, verbose=FALSE, rownames=FALSE, append=FALSE)
  }
  print(key_fields)			
  .UpdateItemsDataRecursively(project.path, project.data, keys=key_fields, values=NULL )
  
} # end function
