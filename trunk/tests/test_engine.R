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
               BuildFilterWithKeys( c("IT", "101"), sep="==", collapse=" & ", na.rm=TRUE)
               )
   checkEquals(
               "KEY1=='IT' & KEY2=='101'", 
               BuildFilterWithKeys( c("IT", "101"), sep="==", collapse=" & ", na.rm=FALSE)
               )
   checkEquals(
               "KEY1=='IT' & KEY2=='' & KEY3=='101'", 
               BuildFilterWithKeys( c("IT", "", "101"), sep="==", collapse=" & ", na.rm=FALSE)
               )
   checkEquals(
               "KEY1=='IT' & KEY2=='' & KEY3=='101'", 
               BuildFilterWithKeys( c("IT", NA, "101"), sep="==", collapse=" & ", na.rm=FALSE)
               )
   
   checkEquals(
               "KEY1=='IT' & KEY3=='101'", 
               BuildFilterWithKeys( c("IT", "", "101"), sep="==", collapse=" & ", na.rm=TRUE)
               )
   checkEquals(
               "KEY1=='IT' & KEY3=='101'", 
               BuildFilterWithKeys( c("IT", NA, "101"), sep="==", collapse=" & ", na.rm=TRUE)
               )
   
}

test.BuildPeriodRange <- function() {
  checkEquals(
              c("2001-2","2001-3","2002-1","2002-2","2002-3"),
              BuildPeriodRange(c(2001,2), 3, 5)
              )
  checkEquals(
              c("2001-3","2002-1","2002-2","2002-3","2003-1"),
              BuildPeriodRange(c(2001,2), 3, 5, shift=1)
              )
}

test.BuildSQLstmtDeleteRecordsWithKeys <- function() {
  checkEquals(
              "delete from europool_VALUE1  where KEY1='IT' and KEY2='101'",
              BuildSQLstmtDeleteRecordsWithKeys( "europool_VALUE1", c("IT", "101"))
              )

  checkEquals(
              "delete from europool_VALUE1  where KEY1='IT' and KEY2='' and KEY3='101'",
              BuildSQLstmtDeleteRecordsWithKeys( "europool_VALUE1", c("IT", "", "101"))
              )
}

test.BuildKeyNames <- function() {
    checkEquals(
                c("KEY1", "KEY2"),
                BuildKeyNames( c("IT","CAR") )
                )
    checkEquals(
                c("KEY1", "KEY2"),
                BuildKeyNames( c("IT","CAR",""), na.rm=TRUE )
                )
    checkEquals(
                c("KEY1", "KEY2", "KEY3"),
                BuildKeyNames( c("IT","CAR",""), na.rm=FALSE )
                )
    checkEquals(
                c("KEY1", "KEY2"),
                BuildKeyNames( c("IT","CAR","",''), na.rm=TRUE )
                )
    checkEquals(
                c("KEY1", "KEY2", "KEY3", "KEY4"),
                BuildKeyNames( c("IT","CAR","",''), na.rm=FALSE )
                )
    checkEquals(
                c("KEY1", "KEY3"),
                BuildKeyNames( c("IT","","CAR"), na.rm=TRUE )
                )
    checkEquals(
                c("KEY1", "KEY2", "KEY3"),
                BuildKeyNames( c("IT","","CAR"), na.rm=FALSE )
                )
    checkEquals(
                c("KEY2", "KEY3"),
                BuildKeyNames( c("","CAR","FIAT"), na.rm=TRUE )
                )
    checkEquals(
                c("KEY1", "KEY2", "KEY3"),
                BuildKeyNames( c("","CAR","FIAT"), na.rm=FALSE )
                )
}

test.EvalParamString <- function() {
  param <- EvalParamString("n.ahead=8,range=c(-Inf,Inf),NA2value=0,n.min=10,try.models=c('mean','trend','lm','es','arima'),logtransform=FALSE,stepwise=TRUE,formula.right.lm='S*trend+S*trend2',criterion='BestAIC',criterionExcludeMaxGreaterThan=2,negToZero=TRUE,predictInteger=TRUE")

  checkEquals( 12, length(param))

  checkEquals(8, param$n.ahead)
  
  checkEquals(
              c("mean", "trend", "lm", "es", "arima"),
              param$try.models
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

  item.data <- ExtractAndAggregateItemDataFromProjectData(p, c("", "MOTO","DUCATI"))
  checkEquals(
              c(33, 5, 44, 36, 80, 0, 56, 0, 80, 43, 22, 24, 53, 44),
              item.data$VALUE1
              )
  checkEquals(
              c("2001-2", "2002-2", "2003-1", "2003-2", "2004-1"),
              rownames(item.data)[1:5]
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


