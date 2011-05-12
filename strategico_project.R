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

ProjectGetItems <- function(project.name) {
  project.path <- ProjectGetPath(project.name)
  filename <- file.path(project.path, "project_items.Rdata")
  FileExistsOrQuit(filename)
  load(filename)
  project.items
}

ProjectGetData <- function(project.name) {
  project.path <- ProjectGetPath(project.name)
  filename <- file.path(project.path, "project_data.Rdata")
  FileExistsOrQuit(filename)
  load(filename)
  project.data
}

ProjectGetConfig <- function(project.name) {
  project.path <- ProjectGetPath(project.name)
  filename <- file.path(project.path, "project.config")
  
  FileExistsOrQuit(filename)
  ## sourcing priect.config file
  source(filename)

  eval.file <- paste("eval_", project.config$eval.function, ".R", sep="")
  MySource(eval.file)
 
  ##append(project.config, strategico.config)
  project.config
}

ProjectGetList <- function(projects.home = strategico.config$projects.home) {
  dir(projects.home)
}
                            
ProjectGetPath <- function(project.name, projects.home = strategico.config$projects.home) {
  file.path(projects.home, project.name)
}

ProjectGetStatistics <-function(project.name, project.config=NULL, project.items=NULL, project.data=NULL, db.channel) {
  
  stats.rdata <- ProjectGetStatisticsRdata(project.name=project.name, project.config=project.config,
                                           project.items=project.items, project.data=project.data)
  
  stats.db <- ProjectGetStatisticsDB(project.name=project.name, project.config=project.config, db.channel=db.channel)

  stats <- t(as.data.frame(append(stats.rdata,stats.db)))
  colnames(stats) = "VALUE"
  stats
}
  
ProjectGetStatisticsRdata <-function(project.name, project.config=NULL, project.items=NULL, project.data=NULL) {
  if (is.null(project.config)) {
    project.config <- ProjectGetConfig(project.name=project.name)
  }
  if (is.null(project.items)) {
    project.items <- ProjectGetItems(project.name=project.name)
  }
  if (is.null(project.data)) {
    project.data <- ProjectGetData(project.name=project.name)
  }
  n.items    <- nrow(project.items)
  n.data     <- nrow(project.data)
  n.values   <- length(project.config$values)
  levels     <- levels(project.data$PERIOD)
  period.min <- min(levels)
  period.max <- max(levels)
  
  stats <- list(
                keys=paste(project.config$keys, collapse=","),
                values=paste(project.config$values, collapse=","),
                n.data=n.data,
                n.items=n.items,
                n.ts=n.items * n.values,
                ts.length=n.data/n.items,
                period.min=period.min,
                period.max=period.max
                )
  stats
}


ProjectGetUrl <- function(project.name, projects.url = strategico.config$projects.url) {
  paste(projects.url, project.name, sep="/")
}

ProjectImportData <- function(project.name, project.config=NULL, db.channel) {
  if (is.null(project.config))
    project.config <- ProjectGetConfig(project.name=project.name)

  project.R <- paste("project_", project.name, ".R", sep="")
  MySource(project.R)
  
  cmd <- paste(project.name,".importItemsData(project.name=project.name)", sep="")
  result <- eval(parse(text = cmd))
  UpdateItemsData(project.name=project.name, project.data=result, db.channel=db.channel)
}

##input da da csv. 
ProjectImportDataFromCSV <- function(project.name, filename=NULL, KEY=c("KEY1","KEY2"),
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

is.project <- function(project.name) {
  project.name %in% ProjectGetList()
}

is.value <- function(value, project.name=NULL, project.config=NULL) {
  value %in% GetValueNames(project.name=project.name, project.config=project.config)
}
