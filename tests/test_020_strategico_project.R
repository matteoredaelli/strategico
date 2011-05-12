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

test.010.ProjectGetConfig <- function() {
  c <- ProjectGetConfig(project.name)

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

test.020.ProjectImportData <- function() {
  ProjectImportData(project.name=project.name, db.channel=db.channel)
  ## TODO Check if the new files have been created successfully 
}

test.030.ProjectGetItems <- function() {
  project.items <- ProjectGetItems(project.name)
  checkEquals(20,
              nrow(project.items)
              )
  checkEquals(4,
              ncol(project.items)
              )
}

test.040.ProjectGetData <- function() {
  project.data <- ProjectGetData(project.name)
  
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

test.GetProjectTablenamesDB <- function() {
   checkEquals(
               c("sample_items", "sample_V1_results", "sample_V1_summary", "sample_V2_results", "sample_V2_summary"),
               GetProjectTablenamesDB(project.name=project.name, project.config=project.config)
               )
}

test.is.project.name <- function() {
  checkEquals(
              TRUE,
              is.project.name("sample")
              )
  checkEquals(
              FALSE,
              is.project.name("missingproject")
              )
}
