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

Project.CreateDB <- function(project.name, project.config=NULL, db.channel=db.channel) {
  sql.file <- Project.BuildSQLscript(project.name=project.name, project.config=project.config)
  a <- read.table(sql.file, sep="$")
  sql <- paste(a[,1], collapse=" ")
  mysql <- strsplit(sql, ";")
  sapply(mysql, function(s) DB.RunSQLQuery(sql_statement=s, db.channel=db.channel))
}

Project.CreateProjectConfig <- function(project.name, mailto=NULL,
                                       period.start, period.end, period.freq, n.ahead=6,
                                       project.keys, project.values) {
  logdebug( "Generating Project Config file")
  input.file <- file.path(GetTemplatesHome(), paste("project-config-ltp.brew", sep=""))
  logdebug( paste("Template file =", input.file))
  output.file <- Project.GetConfigFullPathFilename(project.name)
  logdebug( paste("Target file =", output.file))
  brew(input.file, output=output.file)
  return(0)
}


Project.CreateProjectConfigFromCSVData <- function(project.name, data, mailto, n.ahead) {
  
  if (is.null(data) ) {
    logerror( paste("No data found, no config file will be created for project", project.name))
    return(1)
  }
  
  if (nrow(data) < 2) {
    logerror( paste("No rows found, no config file will be created for project", project.name))
    return(2)
  } 

  header <- colnames(data)

  period.field <- grep("^PERIOD$", header)
  if (length(period.field) == 0) {
    logerror( paste("No PERIOD field found, no config file will be created for project", project.name))
    return(5)
  }
  
  project.keys <- grep("^KEY\\d$", header, value=TRUE)
  if (length(project.keys) == 0) {
    logerror( paste("No KEY1,.. fields found, no config file will be created for project", project.name))
    return(10)
  }

  key.numbers <- sort(as.numeric(gsub("KEY","", project.keys)))
  if (!all(1:length(key.numbers) == key.numbers)) { 
    logerror( paste("Missing/Invalid KEYx field, no config file will be created for project", project.name))
    return(11)
  }
  logwarn( paste("Project keys are:", paste(project.keys, collapse=", ", sep= "")))
  
  project.values <- grep("^V\\d$", header, value=TRUE)
  if (length(project.values) == 0) {
    logerror( paste("No V1,.. fields found, no config file will be created for project", project.name))
    return(20)
  }

  value.numbers <- sort(as.numeric(gsub("V","", project.values)))
  if (!all(1:length(value.numbers) == value.numbers)) { 
    logerror( paste("Missing/Invalid Vx field, no config file will be created for project", project.name))
    return(21)
  }
  logwarn( paste("Project Values are:", paste(project.values, collapse=", ", sep= "")))
  
  period.start.string <- min(as.character(data$PERIOD))
  logwarn( paste("period.start =", period.start.string))
  
  period.start <- ltp::Period.FromString(period.start.string)

  period.end.string <- max(as.character(data$PERIOD))
  logwarn( paste("period.end =", period.end.string))
  period.end <- ltp::Period.FromString(period.end.string)

  if (period.start.string >= period.end.string) { 
    logerror( paste("invalid periods: period.start must be lower than period.end", project.name))
    return(22)
  }
  
  period.freq <- as.integer(max(sapply(strsplit(as.character(data$PERIOD), "-"), function(x) as.integer(x[2]))))
  if (period.freq < 1) { 
    logerror( paste("invalid periods: period.freq (", period.freq, ") must be >= 1", project.name))
    return(22)
  }
  logwarn( paste("period.freq =", period.freq))
  
  Project.CreateProjectConfig(project.name, mailto=mailto,
                              period.start=period.start, period.end=period.end, period.freq=period.freq,
                              n.ahead=n.ahead,
                              project.keys=project.keys, project.values=project.values)
 
}

Project.BuildSQLscript <- function(project.name, project.config=NULL) {
  logdebug( "Generating SQL script")
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
  }
  project.keys <- GetKeyNames(project.name=project.name, project.config=project.config)
  project.values <- GetValueNames(project.name=project.name, project.config=project.config)
  input.file <- file.path(GetTemplatesHome(), paste("project-sql-", "ltp", ".brew", sep=""))
  logdebug( paste("Template file =", input.file))
  output.file <- file.path(Project.GetPath(project.name), paste(project.name, ".sql", sep=""))
  logdebug( paste("Target/SQL file =", output.file))
  brew(input.file, output=output.file)
  output.file
}

Project.DropDB <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
  }

  tables <- Project.GetTableNames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DB.DropTable(x,db.channel))

  views <- Project.GetViewNames(project.name=project.name, project.config=project.config)
  lapply(views, function(x) DB.DropView(x,db.channel))
}

Project.EmptyDB <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
  }

  tables <- Project.GetTableNames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DB.EmptyTable(x,db.channel))
}

Project.EmptyFS <- function(project.name, recursive = TRUE) {
  if (!Project.Exists(project.name)) {
    logwarn( paste("Project folder=", project.name, "doesn't exist"))
  } else {
    project.path <- paste(Project.GetPath(project.name), "/", sep="")
    loginfo( paste("Deleting project path:", project.path))
    unlink(project.path, recursive=recursive)
    project.path <- paste(Project.GetPath(project.name), "/*results*.csv", sep="")
    loginfo( paste("Deleting project files:", project.path))
    unlink(project.path, recursive=FALSE)
    
    ##project.path <- paste(Project.GetPath(project.name), "/project*", sep="")
    ##loginfo( paste("Deleting project files:", project.path))
    ##unlink(project.path)
  }
}

Project.ExportResultsCSV <- function(project.name, project.config=NULL, value, db.channel, file) {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
  }
  if(is.null(file)) {
    name <- paste(project.name, "-results-", value, ".csv", sep="")
    file <- file.path(Project.GetPath(project.name), name)
    logwarn( paste("Missing file option: assuming file =", file))
  }
  results <- Project.GetResults(project.name=project.name, value=value, db.channel=db.channel)
  write.table(results, file=file, row.names=FALSE, append=FALSE, 
              col.names=TRUE, sep=project.config$csv.sep, 
              dec=project.config$csv.dec, quote=TRUE)
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
  
  if (!is.null(where.condition) && where.condition != "")
    where.condition <- paste("where", where.condition, sep=" ")
  
  sql_statement <- paste("select item_id from", tablename, where.condition, sep=" ")
  records <- DB.RunSQLQuery(sql_statement, db.channel=db.channel)
  
  tot <- nrow(records)
  if (tot == 0) {
    logwarn( paste("No id found for KEYS", keys, sep=' ', collapse=','))
    result = NA
  } else {
    result = records$item_id
  }
  result
}

Project.ConfigFile.Exist <- function(project.name) {
  file.exists(Project.GetConfigFullPathFilename(project.name))
}

Project.GetConfigFilename <- function(project.name) {
  paste(project.name, ".config", sep="")
}

Project.GetConfigFullPathFilename <- function(project.name) {
  project.path <- Project.GetPath(project.name)
  filename <- file.path(project.path, Project.GetConfigFilename(project.name))
}

Project.CSVFile.Exist <- function(project.name) {
  file.exists(Project.GetCSVFullPathFilename(project.name))
}
  
Project.GetCSVFilename <- function(project.name) {
  paste(project.name, ".csv", sep="")
}

Project.GetCSVFullPathFilename <- function(project.name) {
  project.path <- Project.GetPath(project.name)
  filename <- file.path(project.path, Project.GetCSVFilename(project.name))
}

Project.GetConfig <- function(project.name, quit=FALSE) {
  default.project.config <- file.path(strategico.config$projects.home, "project.config")
  logdebug( paste("Reading default config file", default.project.config))
  source(default.project.config)

  plugins.path <- GetPluginsPath()
  ##project.path <- Project.GetPath(project.name)
  
  filename <- Project.GetConfigFullPathFilename(project.name)
  logdebug( paste("Reading config file", filename))
  if (!file.exists(filename)) {
    logdebug( paste("... missing config file", filename))
  } else {
    try(source(filename))
  }
  eval.file <- paste("eval_", project.config$eval.function, ".R", sep="")
  eval.file <- file.path(GetPluginsPath(), eval.file)
  logdebug( paste("Reading eval file", eval.file))
  if (! file.exists(eval.file)) {
    loginfo( paste("... missing eval file", eval.file))
    loginfo( "You MUST add it!!!!!!!!!!!!!!!!!!! and quit right now...")
  } else {
    try(source(eval.file))
  }
  project.config
}

Project.GetExpectedCSVHeader <- function(project.name=NULL, project.config=NULL) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)
  
  project.keys <- Project.GetKeys(project.name, project.config=project.config)
  project.values <- Project.GetValues(project.name, project.config=project.config)
  expected.header <- c(project.keys, "PERIOD", project.values)
  logdebug( paste("Expected CSV Header:", paste(expected.header, collapse=", ", sep=" ")))
  expected.header
}
  
Project.GetKeys <- function(project.name=NULL, project.config=NULL) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)
 
  BuildKeyNames(project.config$keys, na.rm=FALSE) 
}

Project.GetValues <- function(project.name=NULL, project.config=NULL) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)
 
  GetValueNames(values=project.config$values) 
}

Project.GetList <- function(projects.home = strategico.config$projects.home) {
  logdebug( paste("Projects in", projects.home, ":"))
  projects <- dir(projects.home)
  logdebug( paste(projects, collapse=", "))
  projects
}
  
Project.GetMaxID <- function(project.name, verbose=FALSE, db.channel) {
  tablename = DB.GetTableNameProjectItems(project.name)
  ##sql_statement <- paste("select max(0 + id) from ", tablename, sep="")
  tot <- DB.GetTableSize(tablename, db.channel)

  logdebug( paste("Max ID =", tot))
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
                      DB.GetTableNameNormalizedData(project.name, value),
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
  tables <- append(tables, paste("v", project.name, "results", sep="_"))
  
  for (value in GetValueNames(project.config$values)) {
    value.tables <- c(
                      paste("v_", DB.GetTableNameResults(project.name, value), sep=""),
                      paste("v_", DB.GetTableNameSummary(project.name, value), sep="")
                      )
    tables <- append(tables, value.tables)
  }
  tables
}

Project.GetUrl <- function(project.name, projects.url = strategico.config$projects.url) {
  paste(projects.url, project.name, sep="/")
}

  
Project.ImportFromCSV <- function(project.name, project.config=NULL, db.channel, filename=NULL, mailto='nobody@localhost', n.ahead=6) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)

  if (is.null(filename))
    filename <- Project.GetCSVFullPathFilename(project.name=project.name)

  logwarn( paste("Loading data from file", filename))
  data=read.table(filename, sep=project.config$csv.sep, dec=project.config$csv.dec, quote=project.config$csv.quote, header=TRUE) 
  logwarn( "CSV file header:")
  csv.header <- colnames(data)
  logwarn( paste(csv.header, collapse=", ", sep=" "))
  
  project.config.file <- Project.GetConfigFullPathFilename(project.name)
  if (file.exists(project.config.file)) {
    logwarn( paste("Config file", project.config.file, "already exists: I'll not create it"))
  } else {
    logwarn( paste("Config file", project.config.file, "doesn't exist: I'll create it for you"))
    result <- Project.CreateProjectConfigFromCSVData(project.name, data=data, mailto=mailto, n.ahead=n.ahead)
    if (result != 0) {
      loginfo( "Something went wrong: not loaing data to DB")
      return (result)
    }
    ## reloading the new project config file
    project.config <- Project.GetConfig(project.name=project.name)
  }
  
  ## creating DB tables
  Project.CreateDB(project.name=project.name,
                   project.config=project.config, db.channel=db.channel)

  ## importing csv data to DB
  Project.Items.UpdateData(project.name=project.name, project.data=data, db.channel=db.channel)
  return (0)
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
  ! (project.name %in% c("strategico"))
}

Project.Exists <- function(project.name) {
  project.name %in% Project.GetList()
}

is.value <- function(value, project.name=NULL, project.config=NULL) {
  value %in% GetValueNames(project.name=project.name, project.config=project.config)
}

## creates item.Rdata e item-list
Project.Items.UpdateData <- function(project.name, project.data, db.channel) {
  if (is.null(project.data)) {
    logwarn( "Project data is empty: No records. Maybe there is something wrong. No data to import to DB/Rdata")
    return(2)
  }
  rows <- nrow(project.data)
  logwarn( paste("Found ", rows, "records!"))
  
  if (rows == 0) {
    logwarn( "No records. Maybe there is something wrong. No data to import to DB/Rdata")
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
    loginfo( "Finding gitems")
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
  logwarn( "Saving project items")
  dbWriteTable(value=project.items, name=tablename, conn=db.channel, append=T, row.names=FALSE)
  
  project.items <- project.items.orig

  ## Putting item ID inside project.data
  ## project.data.db <- merge(project.items, project.data)
  tablename = DB.GetTableNameProjectData(project.config$project.name)
  DB.EmptyTable(tablename, db.channel)
  logwarn( "Saving project data")
  dbWriteTable(value=project.data, name=tablename, conn=db.channel, append=T, row.names=FALSE)
} # end function


Project.NormalizeName <- function(project.name=NULL) {
 ##n <- ifelse (is.null(project.name), "changeme", project.name)
 if(is.null(project.name))
   return("changeme")
 n <- project.name
 n <- substr(n, 1,10)
 n <- tolower(n)
 n <- gsub("[^a-z0-9]", 'x', n)
 n <- gsub('strategico', 'strategic0', n)
 n <- ifelse(n == "", "changeme", n)
 n
}
