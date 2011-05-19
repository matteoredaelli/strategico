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
              12,
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
              c("VendutoPirelli", "VendutoMercato"),
              as.vector(c$values)
              )
  
  checkEquals(
              8,
              c$param$n.ahead
              )

  checkEquals(
              c("mean", "trend", "lm", "es", "arima"),
              c$param$try.models
              )
}

test.020.Project.ImportData <- function() {
  ##Project.FS.Empty(project.name)
  Project.DB.Empty(project.name, db.channel=db.channel)
  Project.ImportData(project.name=project.name, db.channel=db.channel)
  ## TODO Check if the new files have been created successfully 
}

test.030.Project.GetItems <- function() {
  project.items <- Project.GetItems(project.name)
  checkEquals(20,
              nrow(project.items)
              )
  checkEquals(4,
              ncol(project.items)
              )
}

  
test.030.DB.GetTableSize <- function() {
  tablename <- DB.GetTableNameProjectItems("sample")
  checkEquals(20,
              DB.GetTableSize(tablename, db.channel=db.channel)
              )
}

test.040.Project.GetData <- function() {
  project.data <- Project.GetData(project.name)
  
  checkEquals(
              c("KEY1", "KEY2", "KEY3", "PERIOD", "V1", "V2"),
              colnames(project.data)
              )
  checkEquals(113,
              nrow(project.data)
              )
  checkEquals(6,
              ncol(project.data)
              )
}

test.Project.DB.GetTableNames <- function() {
   checkEquals(
               c("sample_data", "sample_items", "sample_results_V1", "sample_summary_V1", "sample_results_V2", "sample_summary_V2"),
               Project.DB.GetTableNames(project.name=project.name, project.config=project.config)
               )
}

test.Project.IsValidName <- function() {
  checkEquals(
              TRUE,
              Project.IsValidName("sample")
              )
  checkEquals(
              FALSE,
              Project.IsValidName("missingproject")
              )
}
