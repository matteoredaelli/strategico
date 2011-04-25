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

library(RODBC)
library("futile.logger")

source("strategico.config")

config_logger(threshold = STRATEGICO$logger.threshold)
logger <- getLogger()


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
  gsub(" ","",gsub("\"","'",paste(names(param),param,sep="=",collapse=",")))
}

BuildPeriodRange <- function(period.start, period.freq, n, shift=0) {
  sapply ((0+shift):(n+shift-1), function(i) paste(.incSampleTime(now=period.start, period.freq = period.freq, increment = i),collapse="-"))
}

EvalItems <- function(project.name, id.min, id.max, keys=NULL, values, param=NULL, CONFIG) {
  for (id in as.integer(id.min):as.integer(id.max)) {
    EvalItem(project.name=project.name, id=id, keys=keys, values=values, param=param, CONFIG=CONFIG)
  }
}

EvalItem <- function(project.name, id=NULL, keys=NULL, values, param=NULL, CONFIG) {
  for (i in 1:length(values)) {
    value <- values[i]
    EvalItemData(project.name=project.name, id=id, keys=keys, value=value, param=param, CONFIG=CONFIG)
  }
}

EvalItemData <- function(project.name, id=NULL, keys=NULL, item.data=NULL, value, param=NULL, CONFIG) {
  logger(INFO, "++++++++++++++++++++++++EvalItemData ++++++++++++++++++++++++")
  logger(INFO, paste("Project=", project.name, " Loading item ID=", id,
                     " KEYS=", paste(keys,collapse=","), " ",
                     value, "=", CONFIG$values[value],
                     sep=""))
  
  if (is.null(item.data)) {
    item.data <- GetItemData(project.name=project.name, id=id, keys=keys, value=value)
  }
  
  logger(INFO, paste("TS length=", nrow(item.data)))
  print( t(item.data))
  
  logger(INFO, paste("period.start=", paste(CONFIG$period.start, collapse="-"),
                     " period.freq=", CONFIG$period.freq,
                     " period.end=", paste(CONFIG$period.end, collapse="-"),
                     sep=""))
  
  if (is.null(id)) {
    logger(INFO, "ID is null, assigning a new value")
    id <- GetNewID()
  }

  directory = GetItemPath(project.name, id, value)
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  prediction = EvalItemDataByValue(project.name=project.name, id=id, item.data=item.data,
    value=value, output.path=directory, param=param, CONFIG=CONFIG
    )
  logger(INFO, "RESULTS:")
  print(t(prediction))
  t(prediction)
}

EvalItemsFromDB <- function(project.name, value, verbose=FALSE, CONFIG) {
  tablename = GetDBTableNameItemSummary(project.name, value)
  sql_statement <- paste("select * from ", tablename, " where Run=1", sep="")
  items <-RunSQLQueryDB(sql_statement)
  summary(items)
  idparam = which(names(items)=="Parameters")
  idKEYs = grep("id",names(items))

  for( i in 1:dim(items)[2]) {
    logger(WARN, items[i,idKEYs]);
    if( (all(is.na(items[i,idKEYs]))) | (!all(sapply( items[i,idKEYs][!is.na(items[i,idKEYs])], is.character )) ))
      return(NA)
    else {
      logger(WARN, items[i,idparam])
      EvalItem(project.name, id=items[i,idKEYs], CONFIG=CONFIG, value=value,
               param=eval(parse(text=paste("list(", gsub("Parameters='","", items[i,idparam]),")",sep="")))  )
    }
  }
}

EvalParamString <- function(param.string) {
  eval(parse(text=paste("param=list(",param.string,")")))
  param
}

EvalTS <- function(project.name, id=NULL, ts.values, ts.periods, period.start, period.freq,
                   calculate.period.end=TRUE, param=NULL, CONFIG, value="VALUE1" ) {
  item.data <- cbind(ts.values)
  rownames(item.data) <-ts.periods
  colnames(item.data) <- c(value)

  CONFIG$period.start <-  period.start
  
  CONFIG$period.freq = period.freq
  
  if (calculate.period.end) {  
    period.end.string <- rownames(item.data)[ length(ts.periods)]
    period.end <- unlist(lapply(strsplit(period.end.string, "-"), as.numeric))
    CONFIG$period.end = period.end
  } # otherwise the project config value will be used
  
  EvalItemData(project.name=project.name, id=id, item.data=item.data, value=value, param=param, CONFIG=CONFIG)
}

EvalTSString <- function(project.name, id=NULL, ts.string,
                         ts.periods.string=NULL, period.start.string, period.freq,
                         calculate.period.end=TRUE, param=NULL, CONFIG) {

  ts.values <- unlist(lapply(strsplit(ts.string,","), as.numeric))

  period.start <- PeriodStringToVector(period.start.string)
  period.freq <- as.integer(period.freq)

  if(is.character(ts.periods.string)) {
    ts.periods.tmp <- unlist(lapply(strsplit(ts.periods.string, ","), as.character))
    if (length(ts.periods.tmp == length(ts.values)))
      ts.periods = ts.periods.tmp
    else
      logger(INFO, "Skipping ts.periods string, not matching length with ts string")
  } else {
    ts.periods <- BuildPeriodRange(period.start, period.freq, length(ts.values))
  }
  

  
  EvalTS(project.name, id=id, ts.values=ts.values, ts.periods=ts.periods, period.start=period.start,
         period.freq=period.freq, calculate.period.end=calculate.period.end, param=param, CONFIG=CONFIG)
}

ExportDataToDB <- function(data, tablename, id.name="id", id=NULL, verbose=FALSE,
                           rownames=FALSE, append=TRUE, addPK=FALSE) {
  
  channel <- odbcConnect(STRATEGICO$db.name, STRATEGICO$db.user, STRATEGICO$db.pass, believeNRows=FALSE)

  delete_sql <- paste("delete from", tablename)
  
  if(!is.null(id)) 
    delete_sql<- paste(delete_sql, "where", id.name, "=", id, sep=" ")

  logger(DEBUG, delete_sql)
  sqlQuery(channel, delete_sql)
 

  sqlSave(channel, data, tablename=tablename, rownames=rownames,
          append=append, verbose=verbose, addPK=addPK, fast=FALSE)
  odbcClose(channel)
}

FixDBProjectTablesStructure <- function(project.name, values) {
  ## TODO: generalize tablenames..
  sql <- c("alter table sample_items MODIFY id integer",
           "alter table sample_VALUE1_results MODIFY item_id integer",
           "alter table sample_VALUE2_results MODIFY item_id integer",
           "alter table sample_VALUE1_results MODIFY id varchar(50)",
           "alter table sample_VALUE2_results MODIFY id varchar(50)",
           "alter table sample_VALUE1_summary MODIFY id integer",
           "alter table sample_VALUE2_summary MODIFY id integer"
            )
  RunSQLQueryDB(sql)         
}

GetDBTableNameItemResults <- function(project.name, value) {
  paste(project.name, value, "results", sep="_")
}

GetDBTableNameItemSummary <- function(project.name, value) {
  paste(project.name, value, "summary", sep="_")
}

GetDBTableNameProjectData <- function(project.name) {
  paste(project.name, "items_data", sep="_")
}

GetDBTableNameProjectItems <- function(project.name) {
  paste(project.name, "items", sep="_")
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
    logger(INFO, "Missing VALUE parameter in GetItemData")
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
  as.integer(records[1,]["id"])
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
    logger(INFO, paste("No keys found for ID=", id))
    result <- NULL
  }
  result
}

GetItemPath <- function(project.name, id, value="") {
  project.path <- GetProjectPath(project.name) 
  path <- paste(project.path, as.integer(id / 500), "/", id, sep="/")
  if( !is.null(value) )
    path <- paste(path, value, sep="/")
  path
}

GetItemResultsDB <- function(project.name, value, id) {
  tablename <- GetDBTableNameItemResults(project.name, value=value)
  GetItemRecordsFromDB(project.name, id, tablename)
}
  
GetItemRecordsFromDB <- function(project.name, id, tablename) {
  filter <- paste("id=", id, sep="")
  sql_statement <- paste("select * from", tablename, "where", filter, sep=" ")
  logger(WARN, sql_statement)
  RunSQLQueryDB(sql_statement)
}
  
GetItemSummaryDB <- function(project.name, value, id) {
  tablename <- GetDBTableNameItemSummary(project.name, value=value)
  GetItemRecordsFromDB(project.name, id, tablename)
}

GetNewID <- function(min=500000, max=502000) {
  sample(min:max,1)
}

GetProjectItems <- function(project.name) {
  project.path <- GetProjectPath(project.name)
  filename <- paste( project.path, "project_items.Rdata", sep="/")
  load(filename)
  project.items
}

GetProjectData <- function(project.name) {
  project.path <- GetProjectPath(project.name)
  load( paste(project.path, "project_data.Rdata", sep="/"))
  project.data
}

GetProjectConfig <- function(project.name) {
  project.path <- GetProjectPath(project.name)
  fileName <- paste(project.path, "project.config", sep="/")
  
  ## cerca il file nella cartella : getwd()
  conf=read.table(fileName, head=FALSE,sep=":",stringsAsFactors =FALSE,quote="\"")
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
  
  ##append(CONFIG, STRATEGICO)
  CONFIG
}

GetProjectPath <- function(project.name, projects.home = STRATEGICO$projects.home) {
  paste(projects.home, project.name, sep="/")
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


ImportProjectData <- function(project.name) {
  if(!exists("CONFIG")) assign("CONFIG", GetProjectConfig(paste(project.name)), envir = .GlobalEnv)
  connector.importItemsData(project.name)
}

##input  da db. 
ImportProjectDataFromDB <- function(project.name, DB, DBUSER, DBPWD, sql_statement ) {
  result <- RunSQLQueryDB(sql_statement, DB, DBUSER, DBPWD)
  UpdateItemsData(project.name, result)
}

##input da da csv. 
ImportProjectDataFromCSV <- function(project.name, filename=NULL, KEY=c("KEY1","KEY2"),
                                     timesKeys=c("YEAR","SEMESTER"), VALUE=c("CORP")){ 

  ##restituisce una list (itemList) con una ts per ogni elemento. 
  ##names(itemList) è una parola composta dai valori assunti nei campi indicati da keys. separatore "[" 
  ##torna utile in seguito, nelle creazioni degli output dell'analisi
  
  if (is.null(filename)) filename=file.choose()
  data=read.csv(filename,sep=",") 
  
  if(length(timesKeys)>1) data$PERIOD=paste(data[,timesKeys[1]],data[,timesKeys[2]],sep="-")
  else data$PERIOD=data[,timesKeys]

  UpdateItemsData(project.name, data[,c(KEY,"PERIOD",VALUE)])
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

RunSQLQueryDB <- function(sql_statements, db=STRATEGICO$db.name, user=STRATEGICO$db.user, pass=STRATEGICO$db.pass) {
  channel <- odbcConnect(db, user, pass, believeNRows=FALSE)
  for (statement in sql_statements)
    result <- sqlQuery(channel, statement)
  odbcClose(channel)

  result
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
UpdateItemsData <- function(project.name, project.data) {
  project.path <- GetProjectPath(project.name)
  
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

  if("items_csv"%in%CONFIG$save)
    write.csv(project.items,
              file= paste(project.path, "/project_items.csv", sep=""),
              row.names = FALSE
              )
  if("items_db"%in%CONFIG$save) {
    tablename = GetDBTableNameProjectItems(CONFIG$project.name)
    ## preparing data for prymary key in DB  (id must be the rownames)
    project.items.orig <- project.items
    rownames(project.items) <- project.items$id
    project.items$id <- NULL
    ExportDataToDB(project.items, tablename, id=NULL, rownames="id", addPK=TRUE)
    project.items <- project.items.orig
  }

  ## Putting item ID inside project.data
  project.data <- merge(project.items, project.data)
  
  outfile <- paste(project.path, "project_data.Rdata", sep="/") 
  save(project.data, file=outfile)
  #print(key_fields)			
  #.UpdateItemsDataRecursively(project.path, project.data, keys=key_fields, values=NULL )
  
} # end function
