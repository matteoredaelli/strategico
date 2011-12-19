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

Project.CreateDBTables <- function(project.name, project.config=NULL, db.channel=db.channel) {
  sql.file <- Project.BuildSQLscript(project.name=project.name, project.config=project.config)
  a <- read.table(sql.file, sep="$")
  sql <- paste(a[,1], collapse=" ")
  mysql <- strsplit(sql, ";")
  sapply(mysql, function(s) DB.RunSQLQuery(sql_statement=s, db.channel=db.channel))
}

Project.BuildSQLscript <- function(project.name, project.config=NULL) {
  logger(WARN, "Generating SQL script")
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
  }
  project.keys <- GetKeyNames(project.name=project.name, project.config=project.config)
  project.values <- GetValueNames(project.name=project.name, project.config=project.config)
  input.file <- file.path(GetTemplatesHome(), paste("project-sql-", "ltp", ".brew", sep=""))
  logger(WARN, paste("Template file =", input.file))
  output.file <- file.path(Project.GetPath(project.name), paste(project.name, ".sql", sep=""))
  logger(WARN, paste("Target/SQL file =", output.file))
  brew(input.file, output=output.file)
  output.file
}

Project.EmptyDB <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
  }

  tables <- Project.GetTableNames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DB.EmptyTable(x,db.channel))
}

Project.EmptyFS <- function(project.name, recursive = TRUE) {
  if (!Project.IsValidName(project.name)) {
    logger(WARN, paste("Project folder=", project.name, "doesn't exist"))
  } else {
    project.path <- paste(Project.GetPath(project.name), "/[0-9]*", sep="")
    logger(INFO, paste("Deleting project path:", project.path))
    unlink(project.path, recursive=recursive)
    project.path <- paste(Project.GetPath(project.name), "/*results*.csv", sep="")
    logger(INFO, paste("Deleting project files:", project.path))
    unlink(project.path, recursive=FALSE)
    
    ##project.path <- paste(Project.GetPath(project.name), "/project*", sep="")
    ##logger(INFO, paste("Deleting project files:", project.path))
    ##unlink(project.path)
  }
}

Project.ExportResultsCSV <- function(project.name, value, db.channel, file, sep=";", quote=FALSE) {
  if(is.null(file)) {
    name <- paste(project.name, "-results-", value, ".csv", sep="")
    file <- file.path(Project.GetPath(project.name), name)
    logger(WARN, paste("Missing file option: assuming file =", file))
  }
  results <- Project.GetResults(project.name=project.name, value=value, db.channel=db.channel)
  write.table(results, file=file, row.names=FALSE, append=FALSE, col.names=TRUE, sep=sep, quote=quote)
}

Project.GetResults <- function(project.name, value, db.channel) {
  tablename <- paste("v_", DB.GetTableNameResults(project.name, value), sep='')
  sql_statement <- paste("select * from", tablename)
  DB.RunSQLQuery(sql_statement, db.channel=db.channel)
}

Project.GetKeyValues <- function(key.name, project.name, db.channel) {
  tablename = DB.GetTableNameProjectItems(project.name)
  sql_statement <- paste("select distinct", key.name, "from", tablename, sep=" ")
  records <- DB.RunSQLQuery(sql_statement, db.channel=db.channel)
  sort(records[,1])
}
 
Project.GetIDs <- function(keys, project.name, db.channel, keys.na.rm=FALSE) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)

  tablename = DB.GetTableNameProjectItems(project.name)
  where.condition <- BuildFilterWithKeys(keys, sep="=", collapse=" and ", na.rm=keys.na.rm)
  sql_statement <- paste("select item_id from", tablename, "where", where.condition, sep=" ")
  records <- DB.RunSQLQuery(sql_statement, db.channel=db.channel)
  
  tot <- nrow(records)
  if (tot == 0) {
    logger(WARN, paste("No id found for KEYS", keys, sep=' ', collapse=','))
    result = NA
  } else {
    result = records$item_id
  }
  result
}

Project.GetConfigFilename <- function(project.name) {
  paste(project.name, ".conf", sep="")
}

Project.GetConfigFullPathFilename <- function(project.name) {
  project.path <- Project.GetPath(project.name)
  filename <- file.path(project.path, Project.GetConfigFilename(project.name))
}

Project.GetDataFilename <- function(project.name) {
  paste(project.name, ".csv", sep="")
}

Project.GetDataFullPathFilename <- function(project.name) {
  project.path <- Project.GetPath(project.name)
  filename <- file.path(project.path, Project.GetDataFilename(project.name))
}

Project.GetConfig <- function(project.name, quit=FALSE) {
  plugins.path <- GetPluginsPath()
  project.path <- Project.GetPath(project.name)
  
  filename <- Project.GetConfigFullPathFilename(project.name)
  logger(DEBUG, paste("Reading config file", filename))
  if (file.exists(filename)) {
  ## sourcing project.config file
    source(filename)
    eval.file <- paste("eval_", project.config$eval.function, ".R", sep="")
    MySource(filename=eval.file, file.path=plugins.path)
 
    ##append(project.config, strategico.config)
  } else {
    project.config <- NULL
  }
  project.config
}

Project.GetList <- function(projects.home = strategico.config$projects.home) {
  logger(DEBUG, paste("Projects in", projects.home, ":"))
  projects <- dir(projects.home)
  logger(DEBUG, projects)
  projects
}
  
Project.GetMaxID <- function(project.name, verbose=FALSE, db.channel) {

  tablename = DB.GetTableNameProjectItems(project.name)
  sql_statement <- paste("select max(0 + id) from ", tablename, sep="")
  tot <- DB.GetTableSize(tablename,db.channel)

  logger(DEBUG, paste("Max ID =", tot))
  tot
}
                          
Project.GetPath <- function(project.name, projects.home = strategico.config$projects.home) {
  file.path(projects.home, project.name)
}

Project.GetStatistics <-function(project.name, project.config=NULL, db.channel) {
  
  stats.rdata <- Project.GetStatisticsRdata(project.name=project.name, project.config=project.config)
  stats.db <- Project.GetStatisticsDB(project.name=project.name, project.config=project.config, db.channel=db.channel)

  stats <- list(fs=stats.rdata,
                db=stats.db
                )
  stats
}
  
Project.GetStatisticsRdata <-function(project.name, project.config=NULL) {
  if (is.null(project.config)) {
    project.config <- Project.GetConfig(project.name=project.name)
  }
  
  stats <- list(
                keys=paste(project.config$keys, collapse=","),
                values=paste(project.config$values, collapse=",")
                )
  stats
}

Project.GetStatistics.Models <- function(project.name, value, db.channel) {  
  t <- DB.GetTableNameSummary(project.name, value)
  sql <- paste("select BestModel, count(*) as tot from", t, "group by 1")
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)
}

Project.GetTableNames <- function(project.name, project.config=NULL) {
  if(is.null(project.config)) 
    project.config <- Project.GetConfig(project.name)

  tables <- c(
              DB.GetTableNameProjectData(project.name),
              DB.GetTableNameProjectItems(project.name)
              )

  
  for (value in GetValueNames(project.config$values)) {
    value.tables <- c(
                      DB.GetTableNameResiduals(project.name, value),
                      DB.GetTableNameResults(project.name, value),
                      DB.GetTableNameSummary(project.name, value),
                      DB.GetTableNameSummaryModels(project.name, value)
                      )
    tables <- append(tables, value.tables)
  }
  tables
}

Project.GetViewNames <- function(project.name, project.config=NULL) {
  if(is.null(project.config)) 
    project.config <- Project.GetConfig(project.name)

  tables <- c() 
  
  for (value in GetValueNames(project.config$values)) {
    value.tables <- c(
                      paste("v_", DB.GetTableNameResults(project.name, value), sep="")
                      )
    tables <- append(tables, value.tables)
  }
  tables
}

Project.GetUrl <- function(project.name, projects.url = strategico.config$projects.url) {
  paste(projects.url, project.name, sep="/")
}

Project.ImportDataFromCSV <- function(project.name, project.config=NULL, db.channel, filename=NULL) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)

  if (is.null(filename))
    filename <- Project.GetDataFullPathFilename(project.name=project.name)

  logger(WARN, paste("Loading data from file", filename))
  result=read.csv(filename,sep=",") 
  Project.Items.UpdateData(project.name=project.name, project.data=result, db.channel=db.channel)
}

##input  da db. 
Project.ImportDataFromDB <- function(project.name, db.name, db.user, db.pass, sql_statement) {
  DB.RunSQLQuery(sql_statement=sql_statement, db.name=db.name, db.user=db.user, db.pass=db.pass)
}

Project.IsValid <- function(project.name, db.channel) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)

  ## TODO:
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
  if (is.null(project.data)) {
    logger(WARN, "Project data is empty: No records. Maybe there is something wrong. No data to import to DB/Rdata")
    return(2)
  }
  rows <- nrow(project.data)
  logger(WARN, paste("Found ", rows, "records!"))
  
  if (rows == 0) {
    logger(WARN, "No records. Maybe there is something wrong. No data to import to DB/Rdata")
    return(3)
  }
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
  
  project.items=leaves
  if ("gitems" %in% project.config$save) {
    logger(INFO, "Finding gitems")
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
  ##allvalues.keys <- rep("", length(key_fields))
  ##project.items <- rbind(allvalues.keys, project.items)

  ## adding ID column
  project.items <- cbind(item_id=1:nrow(project.items), project.items)
 

  tablename = DB.GetTableNameProjectItems(project.config$project.name)
  ## preparing data for prymary key in DB  (id must be the rownames)
  project.items.orig <- project.items
  DB.EmptyTable(tablename, db.channel)
  logger(WARN, "Saving project items")
  dbWriteTable(value=project.items, name=tablename, conn=db.channel, append=T, row.names=FALSE)
  
  project.items <- project.items.orig

  ## Putting item ID inside project.data
  ## project.data.db <- merge(project.items, project.data)
  tablename = DB.GetTableNameProjectData(project.config$project.name)
  DB.EmptyTable(tablename, db.channel)
  logger(WARN, "Saving project data")
  dbWriteTable(value=project.data, name=tablename, conn=db.channel, append=T, row.names=FALSE)
} # end function
