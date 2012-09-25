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
  a <- read.table(sql.file, sep="$", quote="|||")
  sql <- paste(a[,1], collapse=" ")
  mysql <- strsplit(sql, ";")
  sapply(mysql, function(s) DB.RunSQLQuery(sql_statement=s, db.channel=db.channel))
}

Project.insertRowDB <- function(project.name, db.channel, config_basic) {
  sql <- sprintf("insert into strategico_projects (name, config_basic) values ('%s',\"%s\")", project.name, config_basic)
  logwarn( sprintf("Inserting project '%s': %s", project.name, sql))
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel) 
}

Project.selectRowDB <- function(project.name, db.channel) {
  sql <- sprintf("select * from strategico_projects where name = '%s'", project.name)
  logwarn( sprintf("Retreiving project'%s': %s", project.name, sql))
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel) 
}

Project.dropRowDB <- function(project.name, db.channel) {
  sql <- sprintf("delete from strategico_projects where name = '%s'", project.name)
  logwarn( sprintf("Dropping project '%s': %s", project.name, sql))
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel) 
}

Project.updateRowDB <- function(project.name, db.channel, config_basic=NULL, config_csv=NULL, param=NULL, csv_rows=NULL, good_rows=NULL) {
  if (!is.null(config_basic)) {
    sql <- sprintf("update strategico_projects set config_basic=\"%s\" where name = '%s'", config_basic, project.name)
    DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel) 
  }
  if (!is.null(config_csv)) {
    sql <- sprintf("update strategico_projects set config_csv=\"%s\" where name = '%s'", config_csv, project.name)
    DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel) 
  }
  if (!is.null(param)) {
    sql <- sprintf("update strategico_projects set param=\"%s\" where name = '%s'", param, project.name)
    DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel) 
  }
  if (!is.null(csv_rows)) {
    sql <- sprintf("update strategico_projects set csv_rows=\"%s\" where name = '%s'", csv_rows, project.name)
    DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel) 
  }
  if (!is.null(good_rows)) {
    sql <- sprintf("update strategico_projects set good_rows=\"%s\" where name = '%s'", good_rows, project.name)
    DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel) 
  }
}


Project.Create <- function(project.name, db.channel) {
  sql <- "
CREATE TABLE IF NOT EXISTS strategico_projects (
  name varchar(15) NOT NULL,
  owner varchar(30) default NULL,
  csv_rows int(11) default 0,
  good_rows int(11) default 0,
  config_basic varchar(1000) default '',
  config_csv varchar(1000) default '',
  param varchar(1000) default '',
  PRIMARY KEY  (name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8
"
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel) 

  records <- Project.selectRowDB(project.name, db.channel)
  
  if (nrow(records) == 1) {
    logwarn(sprintf("The project '%s': already exists", project.name))
    return(1)
  }

  logwarn( sprintf("Creating project '%s'", project.name))

  project.path <- Project.GetPath(project.name)
  logwarn( sprintf("Creating folder '%s'", project.path))
  dir.create(project.path, showWarnings = FALSE)
  Sys.chmod(project.path, "777")

  template.file <- file.path(GetTemplatesHome(), "project-config-basic-ltp.brew")
  config.basic <- paste(capture.output(brew(template.file)),  collapse="\n")
  logdebug(sprintf("Config basic:\n'%s'", config.basic))

  logwarn("Insert project row in strategico_projects DB table")
  Project.insertRowDB(project.name, db.channel, config.basic)
}

Project.CreateProjectConfig <- function(project.name, mailto=NULL,
                                       period.start, period.end, period.freq, n.ahead=6,
                                       project.keys, project.values, db.channel) {
  records <- Project.selectRowDB(project.name, db.channel) 
  if (nrow(records) != 1) {
    logdebug(sprintf("No row found (too many rows) in strartegico_projects table about project '%s'", project.name))
    return(1)
  }

  template.file <- file.path(GetTemplatesHome(), "project-config-csv-ltp.brew")
  config_csv <- paste(capture.output(brew(template.file)),  collapse="\n")
  logdebug(sprintf("Config:\n'%s'", config.csv))

  template.file <- file.path(GetTemplatesHome(), "project-config-param-ltp.brew")
  param <- paste(capture.output(brew(template.file)),  collapse="\n")
  logdebug(sprintf("Param:\n'%s'", param))

  logwarn("Update csv config and param in strategico_projects DB table")
  Project.updateRowDB(project.name, db.channel=db.channel, config_csv=config_csv, param=param)

  return(0)
}


Project.NormalizeInputDataAndCreateProjectConfig <- function(project.name, data, mailto, n.ahead, db.channel) {
  
  loginfo( sprintf("Found %d rows and %d columns", nrow(data), ncol(data)))
  if (is.null(data) ) {
    logerror( paste("No data found, no config file will be created for project", project.name))
    return(NULL)
  }
  
  if (nrow(data) < 2) {
    logerror( paste("No rows found, no config file will be created for project", project.name))
    return(NULL)
  } 

  csv.header <- colnames(data)

  ## converting colnames to uppercase 
  csv.header <- toupper(csv.header)
  colnames(data) <- csv.header

  logwarn( "CSV file header:")
  logwarn( paste(csv.header, collapse=", ", sep=" "))
  
  period.field <- grep("^PERIOD$", csv.header)
  if (length(period.field) == 0) {
    logerror( paste("No PERIOD field found, no config file will be created for project", project.name))
    return(NULL)
  }
  
  logwarn("Removing rows with PERIOD not like [0-9]+-[0-9]+")
  ## removing rows with PERIOD NOT LIKE "[0-9]+-[0-9]+"
  ## sometimes the export from excel generates teh last row like ;;;;
  ## and a NA value apperas in the period column
  data <- data[grep("[0-9]+-[0-9]+", data$PERIOD),]

  logwarn( paste("Tot rows =", nrow(data)))
  ## TODO: also rows with NA in one of KEYx column should be deleted

  project.keys <- grep("^KEY\\d$", csv.header, value=TRUE)
  if (length(project.keys) == 0) {
    logerror( paste("No KEY1,.. fields found, no config file will be created for project", project.name))
    return(NULL)
  }

  key.numbers <- sort(as.numeric(gsub("KEY","", project.keys)))
  if (!all(1:length(key.numbers) == key.numbers)) { 
    logerror( paste("Missing/Invalid KEYx field, no config file will be created for project", project.name))
    return(NULL)
  }
  logwarn( paste("Project keys are:", paste(project.keys, collapse=", ", sep= "")))

  logwarn("Removing strange characters from KEYs columns")
  for (k in project.keys) {
    data[,k] = gsub("[/'\"\\]", "", data[,k])
  }
  
  project.values <- grep("^V\\d$", csv.header, value=TRUE)
  if (length(project.values) == 0) {
    logerror( paste("No V1,.. fields found, no config file will be created for project", project.name))
    return(NULL)
  }

  value.numbers <- sort(as.numeric(gsub("V","", project.values)))
  if (!all(1:length(value.numbers) == value.numbers)) { 
    logerror( paste("Missing/Invalid Vx field, no config file will be created for project", project.name))
    return(NULL)
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
    return(NULL)
  }
  
  period.freq <- as.integer(max(sapply(strsplit(as.character(data$PERIOD), "-"), function(x) as.integer(x[2]))))
  if (period.freq < 1) { 
    logerror( paste("invalid periods: period.freq (", period.freq, ") must be >= 1", project.name))
    return(NULL)
  }
  logwarn( paste("period.freq =", period.freq))
  
  Project.CreateProjectConfig(project.name, mailto=mailto,
                              period.start=period.start, period.end=period.end, period.freq=period.freq,
                              n.ahead=n.ahead,
                              project.keys=project.keys, project.values=project.values, db.channel=db.channel)
  data
}

## TODO missing db.channel

Project.BuildSQLscript <- function(project.name, project.config=NULL) {
  logdebug( "Generating SQL script")
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)
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
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)
  }

  tables <- Project.GetTableNames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DB.DropTable(x,db.channel))

  views <- Project.GetViewNames(project.name=project.name, project.config=project.config)
  lapply(views, function(x) DB.DropView(x,db.channel))
  Project.dropRowDB(project.name, db.channel)
}

Project.EmptyDB <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)
  }

  tables <- Project.GetTableNames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DB.EmptyTable(x,db.channel))
}

Project.EmptyFS <- function(project.name, recursive = TRUE) {
  if (!Project.ExistsFS(project.name)) {
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
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)
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
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)

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

Project.GetConfig <- function(project.name, db.channel, quit=FALSE) {
  project.config <- list()
  param <- list()

  #default.project.config <- file.path(strategico.config$projects.home, "project.config")
  #logdebug( paste("Reading default config file", default.project.config))
  #source(default.project.config)

  plugins.path <- GetPluginsPath()
  ##project.path <- Project.GetPath(project.name)
  
  #filename <- Project.GetConfigFullPathFilename(project.name)
  #logdebug( paste("Reading config file", filename))
  #if (!file.exists(filename)) {
  #  logdebug( paste("... missing config file", filename))
  #} else {
  #  try(source(filename))
  #}
  
  records <- Project.selectRowDB(project.name, db.channel)
  if(nrow(records) != 1) {
    loginfo("Unexpected number of rows in strategico_projects tables")
    return(project.config)
  }
  config.string <- records[1,]$config_basic
  config_csv <- records[1,]$config_csv
  if (!is.null(config_csv) && config_csv != '')
    config.string <- sprintf("%s\n%s", config.string, config_csv)

  param.string <- records[1,]$param

  logdebug( sprintf("Config = %s", config.string))
  logdebug( sprintf("Param = %s", param.string))

  cmd <- paste("project.config$", unlist(strsplit(config.string, "\\n")), sep="", collapse=";")
  cmd <- gsub("\r", "", cmd)
  logdebug(sprintf("evaluating %s", cmd))
  eval(parse(text=cmd))

  if (!is.null(param.string) && param.string != '') {
    cmd <- paste("param$", unlist(strsplit(param.string, "\\n")), sep="", collapse=";")
  cmd <- gsub("\r", "", cmd)
    logdebug(sprintf("evaluating %s", cmd))
    eval(parse(text=cmd))
  }
  project.config$param <- param
    
  eval.file <- paste("eval_", project.config$eval.function, ".R", sep="")
  eval.file <- file.path(GetPluginsPath(), eval.file)
  logdebug( paste("Reading eval file", eval.file))
  if (! file.exists(eval.file)) {
    loginfo( paste("... miyyssing eval file", eval.file))
    loginfo( "You MUST add it!!!!!!!!!!!!!!!!!!! and quit right now...")
  } else {
    try(source(eval.file))
  }
  project.config
}

Project.GetExpectedCSVHeader <- function(project.name=NULL, project.config=NULL) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)
  
  project.keys <- Project.GetKeys(project.name, project.config=project.config)
  project.values <- Project.GetValues(project.name, project.config=project.config)
  expected.header <- c(project.keys, "PERIOD", project.values)
  logdebug( paste("Expected CSV Header:", paste(expected.header, collapse=", ", sep=" ")))
  expected.header
}
  
Project.GetKeys <- function(project.name=NULL, project.config) {
  BuildKeyNames(project.config$keys, na.rm=FALSE) 
}

Project.GetValues <- function(project.name=NULL, project.config=NULL) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)
 
  GetValueNames(values=project.config$values) 
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
  
  stats.rdata <- Project.GetStatisticsProjectData(project.name=project.name, project.config=project.config, db.channel=db.channel)
  stats.db <- Project.GetStatisticsDB(project.name=project.name, project.config=project.config, db.channel=db.channel)

  stats <- list(csv=stats.rdata,
                db=stats.db
                )
  stats
}
  
Project.GetStatisticsProjectData <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)
  }

  stats <- list()

  sql <- sprintf("SELECT * from strategico_projects where name='%s'", project.name)
  records <- try(DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel))
  
  stats$csv_rows <- records[1,]$csv_rows
  stats$good_rows <- records[1,]$good_rows
  
  stats$keys <- paste(project.config$keys, collapse=",")
  stats$values <- paste(project.config$values, collapse=",")
  stats$period.start <- Period.ToString(project.config$period.start)
  stats$period.end <- Period.ToString(project.config$period.end)
  stats$period.freq <- project.config$period.freq

  t.data.raw <- DB.GetTableNameProjectData(project.name)
  sql <- "SELECT min(_V_) _V__min, max(_V_) _V__max, avg(_V_) _V__mean, sum(_V_) _V__sum FROM __TABLE_DATA_RAW__"
  sql <- gsub("__TABLE_DATA_RAW__", t.data.raw, sql)
  for (value in GetValueNames(project.config$values)) {
    sql.v <- gsub("_V_", value, sql)
    records <- try(DB.RunSQLQuery(sql_statement=sql.v, db.channel=db.channel))
    cols <- colnames(records)
    if (nrow(records) == 1)
      for (x in cols)
         stats[[x]] <- records[1,x]
  }

  
  stats
}

Project.GetStatisticsDB <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)
  }

  tables <- Project.GetTableNames(project.name=project.name, project.config=project.config)
  rows <- unlist(lapply(tables, function(x) DB.GetTableSize(x,db.channel)))
  
  stats <- as.list(rows)
  names(stats) <- tables

  t.items <- DB.GetTableNameProjectItems(project.name)
  sql <- "SELECT min(_V_) _V__min, max(_V_) _V__max, avg(_V_) _V__mean, sum(_V_) _V__sum FROM __TABLE_DATA_RAW__"
  for (value in GetValueNames(project.config$values)) {
    ## adding % of predictions
    t.sum <- DB.GetTableNameSummary(project.name, value)
    stats[[paste("perc_predictions", value, sep= "_")]] <- 0
    try(stats[[paste("perc_predictions", value, sep= "_")]] <- stats[[t.sum]] / stats[[t.items]] * 100)
  }
  stats
}

Project.GetStatistics.Models <- function(project.name, value, db.channel) {  
  t <- DB.GetTableNameSummary(project.name, value)
  sql <- paste("select BestModel, count(*) as tot from", t, "group by 1")
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)
}

Project.GetTableNames <- function(project.name, project.config=NULL) {
  if(is.null(project.config)) 
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)

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
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)

  tables <- c() 
  tables <- append(tables, paste("v", project.name, "results", sep="_"))
  
  for (value in GetValueNames(project.config$values)) {
    value.tables <- c(
                      paste("v_", DB.GetTableNameResults(project.name, value), sep=""),
                      paste("v_", DB.GetTableNameSummary(project.name, value), sep=""),
                      sprintf("v_%s_changed_models_%s", project.name, value)
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
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)

  if (is.null(filename))
    filename <- Project.GetCSVFullPathFilename(project.name=project.name)

  logwarn( paste("Loading data from file", filename))
  #data=read.table(filename, sep=project.config$csv.sep, dec=project.config$csv.dec, quote=project.config$csv.quote, header=TRUE)
  data=read.table(filename, sep=project.config$csv.sep, dec=project.config$csv.dec, quote='"', header=TRUE)
  csv.rows <- nrow(data)
  logwarn( sprintf("found %d rows in csv file %s", csv.rows, filename))
  
  data <- Project.NormalizeInputDataAndCreateProjectConfig(project.name, data=data, mailto=mailto, n.ahead=n.ahead, db.channel=db.channel)
  if (is.null(data)) {
    loginfo( "Something went wrong: not loaing data to DB")
    return (data)
  }
  good.rows <- nrow(data)
  logwarn( sprintf("found good %d rows in csv  file %s", good.rows, filename))
  
  ## reloading the new project config file
  project.config <- Project.GetConfig(project.name, db.channel=db.channel)
  
  ## creating DB tables
  Project.CreateDB(project.name=project.name,
                   project.config=project.config, db.channel=db.channel)

  ##saving csv info
  Project.updateRowDB(project.name, db.channel, csv_rows=csv.rows)
  Project.updateRowDB(project.name, db.channel, good_rows=good.rows)

  ## importing csv data to DB
  Project.Items.UpdateData(project.name=project.name, project.data=data, db.channel=db.channel)
  return (0)
}

##input  da db. 
Project.ImportDataFromDB <- function(project.name, db.name, db.user, db.pass, sql_statement) {
  DB.RunSQLQuery(sql_statement=sql_statement, db.name=db.name, db.user=db.user, db.pass=db.pass)
}

Project.IsValidName <- function(project.name) {
  ! (project.name %in% c("strategico"))
}

Project.Exists <- function(project.name) {
  project.name %in% Projects.GetProjects()
}

Project.ExistsFS <- function(project.name) {
  project.name %in% Projects.GetProjectsFS()
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
  
  project.config <- Project.GetConfig(project.name, db.channel=db.channel)

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
    for (i in (ncol(leaves)):1){
      leaves[,i]=""
      leaves= unique(leaves)
      project.items=rbind(project.items,unique(leaves))
    }
  }  

  ## adding ID column
  project.items <- cbind(item_id=nrow(project.items):1, project.items)

  tablename = DB.GetTableNameProjectItems(project.config$project.name)
  ## preparing data for primary key in DB  (id must be the rownames)
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
  rows <- nrow(project.data)
  logwarn( paste("Saving", rows, "records!"))
  dbWriteTable(value=project.data, name=tablename, conn=db.channel, append=T, row.names=FALSE, verbose=TRUE)
} # end function


Project.NormalizeName <- function(project.name=NULL) {
 ##n <- ifelse (is.null(project.name), "changeme", project.name)
 if(is.null(project.name))
   return("changeme")
 n <- project.name
 n <- substr(n, 1,10)
 ##n <- tolower(n)
 n <- gsub("[^a-z0-9]", 'x', n)
 n <- gsub('strategico', 'strategic0', n)
 n <- ifelse(n == "", "changeme", n)
 n
}

Project.BuildSuspiciousItemsHtmlPage <- function(project.name, db.channel, value, project.config=NULL) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)

  body <- sprintf("%s :: suspicious items :: %s<hr />", project.name, value)
  width=800
  height=300
  
  project.values <- Project.GetValues(project.name, project.config=project.config)
  project.keys <- Project.GetKeys(project.name, project.config=project.config)
  
  ## too-high-children

  template.file <- file.path(GetTemplatesHome(), "sql-ltp-too-high-children.brew")
  sql <- paste(capture.output(brew(template.file)),  collapse="\n")
  logdebug(sql)
  records <- DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)
 
  if (nrow(records) > 0) {
    records$item_id <- Item.AddLink(project.name=project.name, value=value, id.list=records$item_id)
    records$parent_id <- Item.AddLink(project.name=project.name, value=value, id.list=records$parent_id) 
    Table <- gvisTable(records, options=list(width=width, height=height))
    b_Table <- paste(capture.output(cat(Table$html$chart)), collapse="\n")
    body <- paste(body, "<h1>Too high predictions</h1>\n", b_Table)
  }

  ## highest values

  template.file <- file.path(GetTemplatesHome(), "sql-ltp-highest-values.brew")
  sql <- paste(capture.output(brew(template.file)),  collapse="\n")
  logdebug(sql)
  records <- DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)


  if (nrow(records) > 0) {
    records$item_id <- Item.AddLink(project.name=project.name, value=value, id.list=records$item_id) 
    Table <- gvisTable(records, options=list(width=width, height=height))
    b_Table <- paste(capture.output(cat(Table$html$chart)), collapse="\n")
    body <- paste(body, "\n<h1>Predictions with higest results</h1>\n", b_Table)
  }

  ## lowest values

  template.file <- file.path(GetTemplatesHome(), "sql-ltp-lowest-values.brew")
  sql <- paste(capture.output(brew(template.file)),  collapse="\n")
  logdebug(sql)
  records <- DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)


  if (nrow(records) > 0) {
    records$item_id <- Item.AddLink(project.name=project.name, value=value, id.list=records$item_id) 
    Table <- gvisTable(records, options=list(width=width, height=height))
    b_Table <- paste(capture.output(cat(Table$html$chart)), collapse="\n")
    body <- paste(body, "\n<h1>Predictions with lowest results</h1>\n", b_Table)
  }
  
  ## highest maxJump
  
  template.file <- file.path(GetTemplatesHome(), "sql-ltp-highest-maxjump.brew")
  sql <- paste(capture.output(brew(template.file)),  collapse="\n")
  logdebug(sql)
  records <- DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)
  
  if (nrow(records) > 0) {
    records$item_id <- Item.AddLink(project.name=project.name, value=value, id.list=records$item_id) 
    Table <- gvisTable(records, options=list(width=width, height=height))
    b_Table <- paste(capture.output(cat(Table$html$chart)), collapse="\n")
    body <- paste(body, "\n<h1>Highest maxJump (excluding BestModel=Naive)</h1>\n", b_Table)
  }
  
  ##  R2 = 1
  
  template.file <- file.path(GetTemplatesHome(), "sql-ltp-R2-equal-1.brew")
  sql <- paste(capture.output(brew(template.file)),  collapse="\n")
  logdebug(sql)
  records <- DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)
  
  if (nrow(records) > 0) {
    records$item_id <- Item.AddLink(project.name=project.name, value=value, id.list=records$item_id) 
    Table <- gvisTable(records, options=list(width=width, height=height))
    b_Table <- paste(capture.output(cat(Table$html$chart)), collapse="\n")
    body <- paste(body, "\n<h1>R2=1 (excluding BestModel=Naive)</h1>\n", b_Table)
  }
  
  ## highest ratio_means
  
  template.file <- file.path(GetTemplatesHome(), "sql-ltp-highest-ratio-means.brew")
  sql <- paste(capture.output(brew(template.file)),  collapse="\n")
  logdebug(sql)
  records <- DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)
  
  if (nrow(records) > 0) {
    records$item_id <- Item.AddLink(project.name=project.name, value=value, id.list=records$item_id) 
    Table <- gvisTable(records, options=list(width=width, height=height))
    b_Table <- paste(capture.output(cat(Table$html$chart)), collapse="\n")
    body <- paste(body, "\n<h1>Highest ratio Means (between historical and predicted values)</h1>\n", b_Table)
  }

  ## saving html file
  
  template.file <- file.path(GetTemplatesHome(), "report_page.brew")
  html.page <- file.path(Project.GetPath(project.name), sprintf("r_suspicious_items_%s.html", value))
  brew(template.file, output=html.page)
}

Project.BuildStatsHtmlPage <- function(project.name, db.channel, value, project.config=NULL) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)

  body <- sprintf("%s :: stats<hr />", project.name)
  width <- 800
  height <- 300
  
  project.keys <- Project.GetKeys(project.name, project.config=project.config)

  stats <- Project.GetStatistics(project.name, project.config=project.config, db.channel=db.channel)
  stats_csv <- stats$csv
  stats_db <- stats$db

  b_csv <- paste(capture.output(print(xtable(t(as.data.frame(stats_csv))), type="html")), collapse="\n")
  body = sprintf("%s<h1>Import stats</h1>\n%s", body, b_csv)
  
  b_db <- paste(capture.output(print(xtable(t(as.data.frame(stats_db))), type="html")), collapse="\n")
  body = sprintf("%s\n<h1>Database stats</h1>\n%s", body, b_db)

  body = sprintf("%s\n<h1>Predictions (models)</h1>\n", body)
  for (value in GetValueNames(project.config$values)) {
    stats.models <- Project.GetStatistics.Models(project.name, value, db.channel)

    if (!is.null(stats.models) && is.data.frame(stats.models) && nrow(stats.models) > 0) {
      M <- gvisPieChart(stats.models, options=list(title=""))

      MG <- gvisGauge(stats.models, options=list(title=value, min=0, max=stats_db[[DB.GetTableNameProjectItems(project.name)]]))
      b_M <- paste(capture.output(cat(M$html$chart)), collapse="\n")
      b_MG <- paste(capture.output(cat(MG$html$chart)), collapse="\n")
      body = sprintf("%s\n<h2>%s</h2>\n%s\n%s", body, value, b_M, b_MG)
    }
  }

  template.file <- file.path(GetTemplatesHome(), "report_page.brew")
  html.page <- file.path(Project.GetPath(project.name), "r_project_stats.html")
  brew(template.file, output=html.page)
}

