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

source("engine.R")

test.GetFields <- function() {
  checkEquals(
              c("KEY1", "KEY2"),
              .GetFields(c("KEY1", "KEY2", "VALUE1", "PERIOD"), "KEY")
              )
}

test.GetFieldsId <- function() {
  checkEquals(
              c(1,2),
              .GetFieldsId(c("KEY1", "KEY2", "VALUE1", "PERIOD"), "KEY")
              )
}

test.GetProjectConfig <- function() {
  c <- GetProjectConfig("projects/sample/project.config")

  checkEquals(
              11,
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
              c("KEY1", "KEY2", "KEY3"),
              names(c$keys)
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

test.GetProjectData <- function() {
  p <- GetProjectData("projects/sample")
  
  checkEquals(
              c("KEY1", "KEY2", "KEY3", "PERIOD", "VALUE1", "VALUE2"),
              colnames(p)
              )
}

test.incSampleTime <- function() {
  checkEquals(
              c(2002,1),
              .incSampleTime(c(2001,2), period.freq = 3, increment = 2)
              )
  checkEquals(
              c(2014,1),
              .incSampleTime(c(2012,2), period.freq = 2, increment = 3)
              )
}

test.BuildFilterWithKeys <- function() {
   checkEquals(
               "KEY1=='IT' & KEY2=='101'", 
               BuildFilterWithKeys( c("KEY1", "KEY2"), c("IT", "101"), sep="==", collapse=" & ")
               )
}

test.BuildPeriodRange <- function() {
  checkEquals(
              c("2001-2","2001-3","2002-1","2002-2","2002-3"),
              BuildPeriodRange(c(2001,2), 3, 5)
              )
  
}

test.BuildSQLstmtDeleteRecordsWithKeys <- function() {
  checkEquals(
              "delete from europool_VALUE1  where KEY1='IT' and KEY2='101'",
              BuildSQLstmtDeleteRecordsWithKeys( "europool_VALUE1", c("KEY1", "KEY2"), c("IT", "101"))
              )
}

