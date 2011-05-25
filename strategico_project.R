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

Project.FS.Empty <- function(project.name, recursive = TRUE) {
  if (!Project.IsValidName(project.name)) {
    logger(WARN, paste("Project folder=", project.name, "doesn't exist"))
  } else {
    project.path <- paste(Project.GetPath(project.name), "/[0-9]*", sep="")
    logger(INFO, paste("Deleting project path:", project.path))
    unlink(project.path, recursive=recursive)
  }
}

Project.GetItems <- function(project.name) {
  project.path <- Project.GetPath(project.name)
  filename <- file.path(project.path, "project_items.Rdata")
  FileExistsOrQuit(filename)
  load(filename)
  project.items
}

Project.GetData <- function(project.name) {
  project.path <- Project.GetPath(project.name)
  filename <- file.path(project.path, "project_data.Rdata")
  FileExistsOrQuit(filename)
  load(filename)
  project.data
}

Project.GetConfigFilename <- function(project.name) {
  paste("p_", project.name, ".config", sep="")
}

Project.GetConfig <- function(project.name) {
  etc.path <- GetEtcPath()
  plugins.path <- GetPluginsPath()
  filename <- file.path(etc.path, Project.GetConfigFilename(project.name))
  
  FileExistsOrQuit(filename)
  ## sourcing project.config file
  source(filename)

  eval.file <- paste("eval_", project.config$eval.function, ".R", sep="")
  MySource(filename=eval.file, file.path=plugins.path)
 
  ##append(project.config, strategico.config)
  project.config
}

Project.GetList <- function(projects.home = strategico.config$projects.home) {
  dir(projects.home)
}
                            
Project.GetPath <- function(project.name, projects.home = strategico.config$projects.home) {
  file.path(projects.home, project.name)
}

Project.GetStatistics <-function(project.name, project.config=NULL, project.items=NULL, project.data=NULL, db.channel) {
  
  stats.rdata <- Project.GetStatisticsRdata(project.name=project.name, project.config=project.config,
                                           project.items=project.items, project.data=project.data)
  
  stats.db <- Project.GetStatisticsDB(project.name=project.name, project.config=project.config, db.channel=db.channel)

  stats <- list(fs=stats.rdata,
                db=stats.db
                )
  stats
}
  
Project.GetStatisticsRdata <-function(project.name, project.config=NULL, project.items=NULL, project.data=NULL) {
  if (is.null(project.config)) {
    project.config <- Project.GetConfig(project.name=project.name)
  }
  if (is.null(project.items)) {
    project.items <- Project.GetItems(project.name=project.name)
  }
  if (is.null(project.data)) {
    project.data <- Project.GetData(project.name=project.name)
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


Project.GetUrl <- function(project.name, projects.url = strategico.config$projects.url) {
  paste(projects.url, project.name, sep="/")
}

Project.ImportData <- function(project.name, project.config=NULL, db.channel) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)

  project.R <- paste(project.name, ".R", sep="")
  MySource(filename=project.R, file.path=GetPluginsPath())
  
  cmd <- paste(project.name,".importItemsData(project.name=project.name)", sep="")
  result <- eval(parse(text = cmd))
  Project.Items.UpdateData(project.name=project.name, project.data=result, db.channel=db.channel)
}

##input da da csv. 
Project.ImportDataFromCSV <- function(project.name, filename=NULL, KEY=c("KEY1","KEY2"),
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

Project.IsValid <- function(project.name, db.channel) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)
 
  if (is.null(project.items))
    project.items <- Project.GetItems(project.name=project.name)

  if (is.null(project.data))
    project.data <- Project.GetData(project.name=project.name)

  ## TODO: check if project.data contains KEY1, KEY2, .. V1, ..
  TRUE
}

Project.IsValidName <- function(project.name) {
  project.name %in% Project.GetList()
}

is.value <- function(value, project.name=NULL, project.config=NULL) {
  value %in% GetValueNames(project.name=project.name, project.config=project.config)
}

## creates item.Rdata e item-list
Project.Items.UpdateData <- function(project.name, project.data, db.channel) {
  project.path <- Project.GetPath(project.name)
  dir.create(project.path, showWarnings = FALSE)
  
  project.config <- Project.GetConfig(project.name=project.name)

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
  if ("gitems" %in% project.config$save) {
    logger("INFO", "Finding gitems")
    ## save also gitems (only key1 values, key1+key2 values, ...
    for (i in (ncol(leaves)):2){
      leaves[,i]=""
      leaves= unique(leaves)
      project.items=rbind(project.items,unique(leaves))
    }
  }  

  ## TODO adding a row with all keys equal to ''
  ## useful as default value for web forms
  ## not added beacuse KEY1 becomes NA...
  ## allvalues.keys <- rep('', length(key_fields))
  ## project.items <- rbind(project.items, allvalues.keys)

  ## adding ID column
  project.items <- cbind(id=1:nrow(project.items), project.items)
  logger("WARN", paste("Saving Project Items to file", outfile))
  save( project.items, file=outfile)

  if ("data_csv" %in% project.config$save) {
    outfile <- paste(project.path, "/project_items.csv", sep="")
    logger("WARN", paste("Saving Project Items to file", outfile))
    write.csv(project.items,
              file=outfile,
              row.names = FALSE
              )
  }
  
  ## SAVING PROJECT_DATA.RDATA
  filename <- paste(project.path, "project_data.Rdata", sep="/")
  logger("WARN", paste("Saving Project data to file", filename))
  save(project.data, file=filename)
  
  if ("data_db" %in% project.config$save) {
    tablename = DB.GetTableNameProjectItems(project.config$project.name)
    ## preparing data for prymary key in DB  (id must be the rownames)
    project.items.orig <- project.items
    rownames(project.items) <- project.items$id
    project.items$id <- NULL
    
    DB.ImportData(project.items, tablename, id=NULL, rownames="id", addPK=TRUE, db.channel=db.channel)
    
    project.items <- project.items.orig

    ## Putting item ID inside project.data
    #project.data.db <- merge(project.items, project.data)
    tablename = DB.GetTableNameProjectData(project.config$project.name)
    ## preparing data for prymary key in DB  (id must be the rownames)  
    DB.ImportData(project.data, tablename, id=NULL, rownames=NULL,
                  addPK=FALSE, db.channel=db.channel)
  }
 
  #print(key_fields)			
  #.Project.Items.UpdateDataRecursively(project.path, project.data, keys=key_fields, values=NULL )
  
} # end function
