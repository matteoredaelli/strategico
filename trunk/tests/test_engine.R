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

test.GetKeyNames <- function() {
    checkEquals(
                c("KEY1", "KEY2"),
                GetKeyNames(2)
                )
}

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

test.ProjectData <- function() {
  p <- GetProjectData("projects/sample")
  
  checkEquals(
              c("KEY1", "KEY2", "KEY3", "PERIOD", "VALUE1", "VALUE2"),
              colnames(p)
              )

  item.data <- ExtractAndAggregateItemDataFromProjectData(p, c("IT","CAR"))
  
  checkEquals(
              c(644.6, 646, 868, 501.2, 620, 290.3, 560, 680, 624.6, 311, 820, 250.6, 640, 440.6, 4560, 660),
              item.data$VALUE1
              )

  checkEquals(
              c("2003-1","2003-2","2004-1","2004-2","2005-1","2005-2","2006-1","2006-2","2007-1","2007-2","2008-1","2008-2","2009-1","2009-2","2010-1","2010-2"),
              rownames(item.data)
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


