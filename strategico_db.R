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

DB.Connect <- function(db.name=strategico.config$db.name
                      ,db.user=strategico.config$db.user
                      ,db.pass=strategico.config$db.pass
                      #,db.case=strategico.config$db.case
                      ) {
  db.channel <- odbcConnect(db.name, db.user, db.pass, believeNRows=FALSE) #, case=db.case)

  if( db.channel == -1)
    Quit("Cannot connect to DB")
  
  db.channel
}

DB.Close <- function(db.channel) {
  odbcClose(db.channel)
}

Project.DB.Empty <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
  }

  tables <- Project.DB.GetTableNames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DB.EmptyTable(x,db.channel))
}

DB.EmptyTable <- function(tablename, db.channel) {
  sql_statement <- paste("truncate", tablename)
  DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
}

Items.DB.EvalFromSummary <- function(project.name, value, verbose=FALSE, project.config=NULL, db.channel) {
  
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)

  tablename = DB.GetTableNameSummary(project.name, value)
  sql_statement <- paste("select * from ", tablename, " where Run=1", sep="")
  items <-DB.RunSQLQuery(sql_statement, db.channel=db.channel)

  tot <- nrow(items)
  if (tot == 0 )
    logger(WARN, "NO items found to be updated!")
  else {
    for( i in 1:tot) {
      item <- items[i,]
      logger(INFO, paste("Found ID=", items$id))
      logger(INFO, paste("Param String:", item$Parameters))
      
      param <- Param.EvalString(as.character(item$Parameters))
      Item.Eval(project.name=project.name, id=item$id, project.config=project.config,
               value=value, param=param, db.channel=db.channel
               )
    } #end for
  } #end if
}

DB.ImportData <- function(data, tablename, id.name="id", id=NULL, verbose=FALSE,
                           rownames=FALSE, append=TRUE, addPK=FALSE, db.channel) {
  delete_sql <- paste("delete from", tablename)
  
  if(!is.null(id)) 
    delete_sql<- paste(delete_sql, "where", id.name, "=", id, sep=" ")

  logger(DEBUG, delete_sql)
  DB.RunSQLQuery(sql_statement=delete_sql, db.channel=db.channel)


  sqlSave(db.channel, data, tablename=tablename, rownames=rownames,
          append=append, verbose=verbose, addPK=addPK, fast=FALSE)
}

Project.DBExportTables2Csv <- function(project.name, project.config=NULL, db.channel, sep=";", dec=",") {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
  }
  project.path <- Project.GetPath(project.name)
  tables <- Project.DB.GetTableNames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DB.ExportTable2Csv(tablename=x,
                                             db.channel=db.channel,
                                             output.file=file.path(project.path, paste(x, ".csv", sep="")),
                                             sep=sep,
                                             dec=dec
                                             )
         )
}

DB.ExportTable2Csv <- function(tablename, db.channel, output.file, sep=";", dec=",") {
  sql_statement <- paste("select * from", tablename)
  records <- DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
  write.table(records, file=output.file, sep=sep, dec=dec)
}

Project.DB.FixStructure <- function(project.name, values, db.channel) {
  ## TODO: generalize tablenames..
  ## Add unique index on keys in project_items table: needed for speed and consistency
  sql <- c("alter table sample_items MODIFY id integer",
           "alter table sample_results_V1 MODIFY item_id integer",
           "alter table sample_results_V1 MODIFY KEY1 varchar(40)",
           "alter table sample_results_V2 MODIFY item_id integer",
           "alter table sample_results_V1 MODIFY id integer",
           "alter table sample_results_V2 MODIFY id integer",
           "alter table sample_summary_V1 MODIFY id integer",
           "alter table sample_summary_V2 MODIFY id integer"
  ## ALTER TABLE europool_dev_items ADD UNIQUE ( KEY1, KEY2, KEY3, KEY4);
  ## ALTER TABLE europool_dev_results_V1 ADD INDEX ( item_id ) ;
)
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)         
}

DB.GetTableNameResults <- function(project.name, value) {
  paste(project.name, "results", value, sep="_")
}

DB.GetTableNameSummary <- function(project.name, value) {
  paste(project.name, "summary", value, sep="_")
}

DB.GetTableNameProjectData <- function(project.name) {
  paste(project.name, "data_raw", sep="_")
}

DB.GetTableNameNormalizedData <- function(project.name, value) {
  paste(project.name, "data_norm", value, sep="_")
}

DB.GetTableNameProjectItems <- function(project.name) {
  paste(project.name, "items", sep="_")
}

DB.GetTableSize <- function(tablename, db.channel) {
  sql_statement <- paste("select count(*) from", tablename)
  records <- DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
  ## TODO: check if the table doen't exist
  if ( is.data.frame(records) )
    result <- as.integer(records[1][1])
  else {
    logger(WARN, paste("cannot count rows of table", tablename))
    result <- "cannot retreive"
  }
  result
}

Item.DB.GetData <- function(project.name, project.config=NULL, project.items=NULL, id=NULL, keys=NULL, value="V1",
                         keys.na.rm=TRUE, period.start=NULL, period.end=NULL, db.channel) {
 
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)
  
  if (is.null(keys)) {
    if (is.null(project.items))
      project.items <- Project.GetItems(project.name=project.name)
    keys <- Item.GetKeys(id=id, project.name=project.name, project.items=project.items)
  }

  n.char <- nchar(project.config$period.freq)                
  if (is.null(period.start)) period.start <- project.config$period.start
  if (is.null(period.end)) period.end <- project.config$period.end
  
  string.period.start <- Period.ToString(period.start, n.char=n.char)
  string.period.end <- Period.ToString(period.end, n.char=n.char)

  filter.key <- BuildFilterWithKeys(key.values=keys, sep="=", collapse=" and ", na.rm=keys.na.rm)
  filter.period <- paste("period >= '", string.period.start, "' and period <= '", string.period.end, "'", sep="")
  
  tablename <- DB.GetTableNameProjectData(project.name)
  sql_statement <- paste("select period, sum(", value, ") as V from", tablename, "where", filter.key, "and", filter.period, "group by period", sep=" ")

  logger(WARN, sql_statement)
  records <- DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
  rownames(records) <- records$period
  records$period <- NULL
  records
}

Item.DB.GetResults <- function(project.name, value, id, db.channel) {
  tablename <- DB.GetTableNameResults(project.name, value=value)
  Item.DB.GetRecords(project.name, key="item_id", id=id, tablename=tablename, db.channel=db.channel)
}
  
Item.DB.GetRecords <- function(project.name, key="id", id, tablename, db.channel) {
  filter <- paste(key, "=", id, sep="")
  sql_statement <- paste("select * from", tablename, "where", filter, sep=" ")
  logger(WARN, sql_statement)
  DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
}

Item.Db.SaveData <- function(id, data, tablename, db.channel) {
  if (nrow(data)==0) {
    logger(WARN, paste("No data to be saved to", tablename)) 
  } else {
    data = cbind(item_id=id, data)
    data$PERIOD = rownames(data)
    ## primary KEY
    rownames(data) <- paste(data$item_id, data$PERIOD, sep="_")
 
    DB.ImportData(data, tablename=tablename, id=id, id.name="item_id", append=TRUE,
                  rownames="id", addPK=TRUE, db.channel=db.channel)
  }
}

Item.DB.GetSummary <- function(project.name, value, id, db.channel) {
  tablename <- DB.GetTableNameSummary(project.name, value=value)
  Item.DB.GetRecords(project.name, id=id, tablename=tablename, db.channel=db.channel)
}

Project.GetStatisticsDB <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
  }

  tables <- Project.DB.GetTableNames(project.name=project.name, project.config=project.config)
  rows <- unlist(lapply(tables, function(x) DB.GetTableSize(x,db.channel)))
  
  stats <- as.list(rows)
  names(stats) <- tables
  stats
}

Project.DB.GetTableNames <- function(project.name, project.config=NULL) {
  if(is.null(project.config)) 
    project.config <- Project.GetConfig(project.name)

  tables <- c(
              DB.GetTableNameProjectData(project.name),
              DB.GetTableNameProjectItems(project.name)
              )

  
  for (value in GetValueNames(project.config$values)) {
    value.tables <- c(
                      DB.GetTableNameNormalizedData(project.name, value),
                      DB.GetTableNameResults(project.name, value),
                      DB.GetTableNameSummary(project.name, value)
                      )
    tables <- append(tables, value.tables)
  }
  tables
}
    
##input  da db. 
Project.DB.ImportData <- function(project.name, db.name, db.user, db.pass, sql_statement) {
  DB.RunSQLQuery(sql_statement=sql_statement, db.name=db.name, db.user=db.user, db.pass=db.pass)
}

DB.RunSQLQuery <- function(sql_statement, db.channel=NULL, db.name=NULL, db.user=NULL, db.pass=NULL) {
  db.channel.old <- db.channel
  if(is.null(db.channel)) {
    if(is.null(db.name))
      db.channel <- DB.Connect()
    else
      db.channel <- DB.Connect(db.name=db.name, db.user=db.user, db.pass=db.pass)
  }

  for (statement in sql_statement) {
    logger(DEBUG, paste("Running SQL:", statement))
    result <- sqlQuery(db.channel, statement)
    ## checking return code
    ##if (is.numeric(result) & result == -1) {
    ##  logget(INFO, odbcGetErrMsg(db.channel))
    ##  odbcClearError(db.channel)
    ##}

  }
  if(is.null(db.channel.old)) {
    odbcClose(db.channel)
  }
  result
}
