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

EvalItemsFromDB <- function(project.name, value, verbose=FALSE, project.config=NULL) {
  
  if (is.null(project.config))
    project.config <- GetProjectConfig(project.name=project.name)

  tablename = GetDBTableNameItemSummary(project.name, value)
  sql_statement <- paste("select * from ", tablename, " where Run=1", sep="")
  items <-RunSQLQueryDB(sql_statement)

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
             value=value, param=param
               )
    } #end for
  } #end if
}

ExportDataToDB <- function(data, tablename, id.name="id", id=NULL, verbose=FALSE,
                           rownames=FALSE, append=TRUE, addPK=FALSE) {
  
  channel <- odbcConnect(strategico.config$db.name, strategico.config$db.user, strategico.config$db.pass, believeNRows=FALSE)

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

GetDBItemResults <- function(project.name, id, value) {
  tablename <- GetDBTableNameItemResults(project.name, value)
  sql_statement <- "select * from _TABLENAME_ where item_id=_ID_"
  sql_statement <- gsub("_TABLENAME_", tablename, sql_statement)
  sql_statement <- gsub("_ID_", id, sql_statement)
  RunSQLQueryDB(sql_statement)
}

GetDBItemSummary <- function(project.name, id, value) {
  tablename <- GetDBTableNameItemSummary(project.name, value)
  sql_statement <- "select * from _TABLENAME_ where id=_ID_"
  sql_statement <- gsub("_TABLENAME_", tablename, sql_statement)
  sql_statement <- gsub("_ID_", id, sql_statement)
  RunSQLQueryDB(sql_statement)
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

GetDBTableSize <- function(tablename) {
  sql_statement <- paste("select count(*) from", tablename)
  records <- RunSQLQueryDB(sql_statement)
  as.integer(records[1][1])
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

##input  da db. 
ImportProjectDataFromDB <- function(project.name, DB, DBUSER, DBPWD, sql_statement ) {
  result <- RunSQLQueryDB(sql_statement, DB, DBUSER, DBPWD)
  UpdateItemsData(project.name, result)
}

RunSQLQueryDB <- function(sql_statements, db=strategico.config$db.name, user=strategico.config$db.user, pass=strategico.config$db.pass) {
  channel <- odbcConnect(db, user, pass, believeNRows=FALSE)
  for (statement in sql_statements) {
    logger(DEBUG, paste("Running SQL:", statement))
    result <- sqlQuery(channel, statement)
  }
  odbcClose(channel)

  result
}
