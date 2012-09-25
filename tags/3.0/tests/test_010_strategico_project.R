## This program is free software: you can redistribute it and/or modify
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

test.010.Project.GetConfig <- function() {
  c <- Project.GetConfig(project.name)

  checkEquals(
              9,
              length(c)
              )

  checkEquals(
              13,
              length(c$param)
              )

  checkEquals(
              "sample",
              c$project.name
              )

  checkEquals(
              3,
              length(c$keys)
              )
  
  checkEquals(
              c("VendutoCorporate", "VendutoMercato"),
              as.vector(c$values)
              )
  
  checkEquals(
              8,
              c$param$n.ahead
              )

  checkEquals(
              c("mean", "trend", "lm", "es", "arima","naive"),
              c$param$try.models
              )
}

test.020.Project.ImportData <- function() {
  ##Project.EmptyFS(project.name)
  Project.EmptyDB(project.name, db.channel=db.channel)
  Project.ImportData(project.name=project.name, db.channel=db.channel)
  ## TODO Check if the new files have been created successfully 
}
  
test.030.DB.GetTableSize <- function() {
  tablename <- DB.GetTableNameProjectItems("sample")
  checkEquals(20,
              DB.GetTableSize(tablename, db.channel=db.channel)
              )
}

test.Project.GetTableNames <- function() {
   checkEquals(
               c("sample_data_raw", "sample_items",
                 "sample_data_norm_V1", "sample_results_V1", "sample_summary_V1", "sample_summary_models_V1",
                 "sample_data_norm_V2", "sample_results_V2", "sample_summary_V2", "sample_summary_models_V2"),
               Project.GetTableNames(project.name=project.name, project.config=project.config)
               )
}

test.Project.Exists <- function() {
  checkEquals(
              TRUE,
              Project.Exists("sample")
              )
  checkEquals(
              FALSE,
              Project.Exists("missingproject")
              )
}


test.035.Project.GetIDs <- function() {

  checkEquals(
              c(2),
              Project.GetIDs(keys=c("IT","CAR","ALFA"), project.name=project.name, keys.na.rm=FALSE, db.channel=db.channel)
              )
  checkEquals(
              c(10),
              Project.GetIDs(keys=c("IT","CAR",""), project.name=project.name, keys.na.rm=FALSE, db.channel=db.channel)
              )
  checkEquals(
              c(16),
              Project.GetIDs(keys=c("IT","",""), project.name=project.name, keys.na.rm=FALSE, db.channel=db.channel)
              )
  checkEquals(
              c(1, 2, 8, 10, 14, 16),
              sort(Project.GetIDs(keys=c("IT","",""), project.name=project.name, keys.na.rm=TRUE, db.channel=db.channel))
              )
}
