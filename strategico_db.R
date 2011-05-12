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

DBConnect <- function(db.name=strategico.config$db.name
                      ,db.user=strategico.config$db.user
                      ,db.pass=strategico.config$db.pass
                      #,db.case=strategico.config$db.case
                      ) {
  db.channel <- odbcConnect(db.name, db.user, db.pass, believeNRows=FALSE) #, case=db.case)

  if( db.channel == -1)
    Quit("Cannot connect to DB")
  
  db.channel
}

DBClose <- function(db.channel) {
  odbcClose(db.channel)
}

ProjectEmptyDBTables <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- ProjectGetConfig(project.name)
  }

  tables <- ProjectGetDBTablenames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DBEmptyTable(x,db.channel))
}

DBEmptyTable <- function(tablename, db.channel) {
  sql_statement <- paste("truncate", tablename)
  DBRunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
}

DBEvalItemsFromSummary <- function(project.name, value, verbose=FALSE, project.config=NULL, db.channel) {
  
  if (is.null(project.config))
    project.config <- ProjectGetConfig(project.name=project.name)

  tablename = DBGetTableNameSummary(project.name, value)
  sql_statement <- paste("select * from ", tablename, " where Run=1", sep="")
  items <-DBRunSQLQuery(sql_statement, db.channel=db.channel)

  tot <- nrow(items)
  if (tot == 0 )
    logger(WARN, "NO items found to be updated!")
  else {
    for( i in 1:tot) {
      item <- items[i,]
      logger(INFO, paste("Found ID=", items$id))
      logger(INFO, paste("Param String:", item$Parameters))
      
      param <- EvalParamString(as.character(item$Parameters))
      EvalItem(project.name=project.name, id=item$id, project.config=project.config,
               value=value, param=param, db.channel=db.channel
               )
    } #end for
  } #end if
}

ExportDataToDB <- function(data, tablename, id.name="id", id=NULL, verbose=FALSE,
                           rownames=FALSE, append=TRUE, addPK=FALSE, db.channel) {
  delete_sql <- paste("delete from", tablename)
  
  if(!is.null(id)) 
    delete_sql<- paste(delete_sql, "where", id.name, "=", id, sep=" ")

  logger(DEBUG, delete_sql)
  DBRunSQLQuery(sql_statement=delete_sql, db.channel=db.channel)


  sqlSave(db.channel, data, tablename=tablename, rownames=rownames,
          append=append, verbose=verbose, addPK=addPK, fast=FALSE)
}

ProjectDBExportTables2Csv <- function(project.name, project.config=NULL, db.channel, sep=";", dec=",") {
  if(is.null(project.config)) {
    project.config <- ProjectGetConfig(project.name)
  }
  project.path <- ProjectGetPath(project.name)
  tables <- ProjectGetDBTablenames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DBExportTable2Csv(tablename=x,
                                             db.channel=db.channel,
                                             output.file=file.path(project.path, paste(x, ".csv", sep="")),
                                             sep=sep,
                                             dec=dec
                                             )
         )
}

DBExportTable2Csv <- function(tablename, db.channel, output.file, sep=";", dec=",") {
  sql_statement <- paste("select * from", tablename)
  records <- DBRunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
  write.table(records, file=output.file, sep=sep, dec=dec)
}

ProjectFixDBStructure <- function(project.name, values, db.channel) {
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
  DBRunSQLQuery(sql_statement=sql, db.channel=db.channel)         
}

DBGetTableNameResults <- function(project.name, value) {
  paste(project.name, "results", value, sep="_")
}

DBGetTableNameSummary <- function(project.name, value) {
  paste(project.name, "summary", value, sep="_")
}

DBGetTableNameProjectData <- function(project.name) {
  paste(project.name, "items_data", sep="_")
}

DBGetTableNameProjectItems <- function(project.name) {
  paste(project.name, "items", sep="_")
}

DBGetTableSize <- function(tablename, db.channel) {
  sql_statement <- paste("select count(*) from", tablename)
  records <- DBRunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
  ## TODO: check if the table doen't exist
  if ( is.data.frame(records) )
    result <- as.integer(records[1][1])
  else {
    logger(WARN, paste("cannot count rows of table", tablename))
    result <- "cannot retreive"
  }
  result
}

ItemDBGetResults <- function(project.name, value, id, db.channel) {
  tablename <- DBGetTableNameResults(project.name, value=value)
  ItemGetDBRecords(project.name, key="item_id", id=id, tablename=tablename, db.channel=db.channel)
}
  
ItemGetDBRecords <- function(project.name, key="id", id, tablename, db.channel) {
  filter <- paste(key, "=", id, sep="")
  sql_statement <- paste("select * from", tablename, "where", filter, sep=" ")
  logger(WARN, sql_statement)
  DBRunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
}
  
ItemDBGetSummary <- function(project.name, value, id, db.channel) {
  tablename <- DBGetTableNameSummary(project.name, value=value)
  ItemGetDBRecords(project.name, id=id, tablename=tablename, db.channel=db.channel)
}

ProjectGetStatisticsDB <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- ProjectGetConfig(project.name)
  }

  tables <- ProjectGetDBTablenames(project.name=project.name, project.config=project.config)
  rows <- unlist(lapply(tables, function(x) DBGetTableSize(x,db.channel)))
  
  stats <- as.list(rows)
  names(stats) <- tables
  stats
}

ProjectGetDBTablenames <- function(project.name, project.config=NULL) {
  if(is.null(project.config)) 
    project.config <- ProjectGetConfig(project.name)

  tables <- c(
              ##DBGetTableNameProjectData(project.name),
              DBGetTableNameProjectItems(project.name)
              )

  
  for (value in GetValueNames(project.config$values)) {
    value.tables <- c(
                    DBGetTableNameResults(project.name, value),
                    DBGetTableNameSummary(project.name, value)
                    )
    tables <- append(tables, value.tables)
  }
  tables
}
    
##input  da db. 
ProjectImportDataFromDB <- function(project.name, db.name, db.user, db.pass, sql_statement) {
  DBRunSQLQuery(sql_statement=sql_statement, db.name=db.name, db.user=db.user, db.pass=db.pass)
}

DBRunSQLQuery <- function(sql_statement, db.channel=NULL, db.name=NULL, db.user=NULL, db.pass=NULL) {
  db.channel.old <- db.channel
  if(is.null(db.channel)) {
    if(is.null(db.name))
      db.channel <- DBConnect()
    else
      db.channel <- DBConnect(db.name=db.name, db.user=db.user, db.pass=db.pass)
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
