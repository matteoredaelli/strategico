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

## project name: strategico
## project website: http://code.google.com/p/strategico/
## created: 2011

suppressPackageStartupMessages(library(tools))
suppressPackageStartupMessages(library(logging))
suppressPackageStartupMessages(library(xtable))
suppressPackageStartupMessages(library(xtable))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(brew))
suppressPackageStartupMessages(library(RMySQL))


Quit <- function (msg="", status=-1, save="no") {
  print(msg)
  print("Bye!")
  q(status=status)
}

FileExistsOrQuit <- function (filename, msg="", status=10, quit=TRUE){
  if (!file.exists(filename)) {
    print( paste("File", filename, "not found!"))
    if (quit) Quit(msg=msg, status=status)   
  }
}

GetEtcPath <- function() {
  path <- file.path(GetStrategicoHome(), "etc")
  FileExistsOrQuit(path)
  path
}

GetPluginsPath <- function() {
  path <- file.path(GetStrategicoHome(), "plugins")
  FileExistsOrQuit(path)
  path
}

GetStrategicoHome <- function() {
  strategico.path <-as.character(Sys.getenv("STRATEGICO_HOME"))
  if (strategico.path == "") {
    print("Environment STRATEGICO_HOME not set, Bye!")
    q(status=2)
  }
  ##print(paste("Environment STRATEGICO_HOME is set to", strategico.path))
  strategico.path
}

GetTemplatesHome <- function() {
  file.path(GetStrategicoHome(), "templates")
}

MySource <- function(filename, file.path=NULL) {
  if(is.null(file.path))
    file.path <- GetStrategicoHome()

  fullname = file.path(file.path, filename)
  FileExistsOrQuit(fullname)
  source(fullname)
}

MySource(filename="strategico.config", file.path=GetEtcPath())

basicConfig()
setLevel(strategico.config$logger.level)

MySource("strategico_project.R")
MySource("strategico_item.R")
MySource("strategico_db.R")
MySource("strategico_period.R")

BuildFullKey <- function(keys, config.keys, fill.with="") {
  append(keys, rep(fill.with, length(config.keys) - length(keys)))
}

## TODO should manage NA (and NULL) as alias of  ''
BuildKeyNames <- function(key.values, na.rm=FALSE) {
  idx = if (na.rm)
    grep('^$', key.values, invert=TRUE)
  else
    seq(1,length(key.values))

  if (length(idx) > 0)
    result <- paste("KEY", idx, sep="")
  else
    result <- NULL

  result
}

BuildFilterWithKeys <- function(key.values, sep="=", collapse=",", na.rm=FALSE) {
  ## a filter can be like "KEY1=='IT' & KEY2=='101'
  ## see runit test file for more samples
  
  key.values[is.na(key.values)] = ''
  key.names <- BuildKeyNames(key.values, na.rm=na.rm)
  if (is.null(key.names))
    result <- ""
  else {
    if (na.rm)
      key.values <- key.values[ key.values != "" & !is.na(key.values)]
    quoted.keys <- gsub("^(.*)$", "'\\1'", key.values)
    result <- paste(key.names, quoted.keys, sep=sep, collapse=collapse)
  }
  result
}

EvalTS <- function(project.name, id=NULL, ts.values, ts.periods, period.start, period.freq,
                   calculate.period.end=TRUE, param=NULL, project.config, value="V1", db.channel ) {
  item.data <- cbind(ts.values)
  rownames(item.data) <-ts.periods
  colnames(item.data) <- "V"

  project.config$period.start <-  period.start
  
  project.config$period.freq = period.freq
  
  if (calculate.period.end) {  
    period.end.string <- rownames(item.data)[ length(ts.periods)]
    period.end <- unlist(lapply(strsplit(period.end.string, "-"), as.numeric))
    project.config$period.end = period.end
  } # otherwise the project config value will be used
  
  Item.EvalData(project.name=project.name, id=id, item.data=item.data, value=value,
               param=param, project.config=project.config, db.channel=db.channel)
}

EvalTSString <- function(project.name, id=NULL, ts.string,
                         ts.periods.string=NULL, period.start.string, period.freq,
                         calculate.period.end=TRUE, param=NULL, project.config=NULL, db.channel) {
  if (is.null(project.config)) {
    project.config <- Project.GetConfig(project.name=project.name)
  }
  ts.values <- unlist(lapply(strsplit(ts.string,","), as.numeric))

  period.start <- Period.FromString(period.start.string)
  period.freq <- as.integer(period.freq)

  ts.periods <- Period.BuildRange(period.start, period.freq, length(ts.values))
  
  if(is.character(ts.periods.string)) {
    ts.periods.tmp <- unlist(lapply(strsplit(ts.periods.string, ","), as.character))
    if (length(ts.periods.tmp) == length(ts.values))
      ts.periods = ts.periods.tmp
    else
      logwarn( "Skipping ts.periods string, not matching length with ts string")
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

GetKeyNames <- function(keys=NULL, project.name=NULL, project.config=NULL) {
  if (is.null(keys)) {
    if (is.null(project.config))
      project.config <- Project.GetConfig(project.name)
   
    keys <- project.config$keys
  }
  paste("KEY", 1:length(keys), sep="")
}

GetStrHTMLformItem.Eval <- function(project.path, item.path, value, param) {
  param.string <- Param.ToString(param)
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

GetUniqueKeyValues <- function(project.name=NULL, project.config=NULL, db.channel) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)

  keys <- paste("KEY", 1:length(project.config$keys), sep="")
  sapply(keys, function(k) Project.GetKeyValues(k, project.name=project.name, db.channel=db.channel))
}

GetValueNames <- function(values=NULL, project.name=NULL, project.config=NULL) {
  if (is.null(values)) {
    if (is.null(project.config))
      project.config <- Project.GetConfig(project.name)
    values <- project.config$values
  }
  paste("V", 1:length(values), sep="")
}

is.value <- function(value, project.name=NULL, project.config=NULL) {
  value %in% GetValueNames(project.name=project.name, project.config=project.config)
}

Strategico.ExecCommand <- function(project.name, cmd, options="", mailto="", intern=TRUE, wait=strategico.config$command.wait) {
  strategico.command <- strategico.config$strategico.command
  strategico.command <- gsub("__PROJECT_NAME__", project.name, strategico.command)
  strategico.command <- gsub("__CMD__", cmd, strategico.command)
  if (!is.null(mailto))
    strategico.command <- gsub("__MAILTO__", mailto, strategico.command)
  strategico.command <- gsub("__OPTIONS__", options, strategico.command)
  logwarn(paste("Running command:", strategico.command))
  rc <- system(strategico.command, wait=wait, intern=intern)
  logdebug(rc)
  rc
}

Strategico.Sendmail <- function(project.name, project.config=NULL, subject, body=" ", to=NULL) {
  library(sendmailR)
  if (is.null(to)) to <- project.config$mailto
  if (!is.null(to)) {
    logdebug( paste("Sending email to", to))
    subject <- paste("strategico", project.name, subject, sep=" :: ")
    sendmail(from=strategico.config$mail.from, to=to, subject=subject, msg=body,
             control=list(smtpServer=strategico.config$mail.smtp))
  }
}

Param.EvalString <- function(param.string) {
  if (is.character(param.string)) {
    param.string <- gsub(";",",", param.string)
    param <- eval(parse(text=paste("list(",param.string,")")))
  } else {
    param <- list()
  }
  param
}

Param.MergeWithDefault <- function(project.name=NULL, project.config=NULL, param) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)
  
  c(param,project.config$param[setdiff(names(project.config$param),names(param))])
}

Param.ToString <- function(param) {
  param <- lapply(param,function(p){if((length(p)==1)&(is.character(p))) p=paste("'",p,"'",sep="") else p })
  param <- param[names(param)!=""]
  gsub(" ","",gsub("\"","'",paste(names(param),param,sep="=",collapse=";")))
}

SubsetByKeys <- function(data, keys, keys.na.rm=TRUE) {
  filter <- BuildFilterWithKeys(keys, sep="==", collapse=" & ", na.rm=keys.na.rm)
  logdebug( filter)
  cmd <- "records <- subset(data, __FILTER__)"
  cmd <- gsub("__FILTER__", filter, cmd)
  eval(parse(text = cmd))
  records
}

SubsetByID <- function(data, id) {
  cmd <- "subset(data, item_id==__ID__)"
  cmd <- gsub("__ID__", id, cmd)
  eval(parse(text = cmd))
}

.Project.Items.UpdateDataRecursively.ununsed <- function(project.path, data, keys, values=NULL, stats=FALSE) {
  if (is.null(values))
    folder <- project.path
  else
    folder <- paste(project.path, paste(values,collapse="/"), sep="/")

  logwarn( folder)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  vals.names <- .GetFields(names(data),"value")
  temp=by(data[,vals.names,drop=FALSE],data$PERIOD, function(x) apply(x,2, sum, na.rm=TRUE))
  item_data <- as.data.frame(t(matrix(unlist(temp),nrow=length(vals.names))))
  rownames(item_data) <- names(temp)[!sapply(temp,is.null)]
  colnames(item_data) <- vals.names
  save(item_data, file= paste(folder, "item.Rdata", sep="/"))
  
  if("data_csv"%in%project.config$save)
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
      
      .Project.Items.UpdateDataRecursively(project.path=project.path, data=newData, keys=newKeys, values=newValues)
    }
  }
}

StrToRange <- function(str) {
  values <- unlist(strsplit(str, ":"), as.numeric)
  as.integer(values[1]):as.integer(values[2])
}

SW.License <- function() {"GPL V3+"}

SW.Description <- function() {
  paste("Strategico - Copyright(c) 2010, 2011
Version: ", SW.Version(),"
License: ", SW.License(),"
Contributors: ",SW.Contributors())
}

SW.Contributors <- function() {"
  Livio Finos
  Matteo Redaelli
  Marco Rinaldo
"
}

SW.Version <- function() {"Release 1.2.0"}

Vector.ToString <- function(values) {
   paste(values, collapse=",")
}
Vector.FromString <- function(string) {
  unlist(strsplit(string, ","))
}

