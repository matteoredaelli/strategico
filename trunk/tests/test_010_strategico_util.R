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

project.name <- "sample"
project.config <- GetProjectConfig(project.name)
db.channel <- DBConnect()

test.010.GetProjectConfig <- function() {
  c <- GetProjectConfig(project.name)

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

test.020.ImportProjectData <- function() {
  ImportProjectData(project.name=project.name, db.channel=db.channel)
  ## TODO Check if the new files have been created successfully 
}

test.030.GetProjectItems <- function() {
  project.items <- GetProjectItems(project.name)
  checkEquals(20,
              nrow(project.items)
              )
  checkEquals(4,
              ncol(project.items)
              )
}

test.035.GetItemsKeys <- function() {
  project.items <- GetProjectItems(project.name)
  k1 <-     GetItemKeys(project.name=project.name, id=5)
  k1.bis <- GetItemKeys(project.items=project.items, id=5)
  
  checkEquals(
              k1,
              k1.bis
              )
  ## checkEquals(
  ##            c("ES", "MOTO", "DUCATI")
  ##            k1[1,]
  ##            ) 
}

test.035.GetItemsID <- function() {
  project.items <- GetProjectItems(project.name)
  
  checkEquals(
              c(2),
              GetItemsID(keys=c("IT","CAR","ALFA"), project.items=project.items, keys.na.rm=FALSE)
              )
  checkEquals(
              c(10),
              GetItemsID(keys=c("IT","CAR",""), project.items=project.items, keys.na.rm=FALSE)
              )
  checkEquals(
              c(16),
              GetItemsID(keys=c("IT","",""), project.items=project.items, keys.na.rm=FALSE)
              )
  checkEquals(
              c(1, 2, 8, 10, 14, 16),
              GetItemsID(keys=c("IT","",""), project.items=project.items, keys.na.rm=TRUE)
              )
}

test.040.GetProjectData <- function() {
  project.data <- GetProjectData(project.name)
  
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

test.050.Subset <- function() {
  project.data <- GetProjectData(project.name)
  s = SubsetByKeys(project.data, keys=c("ES","MOTO","DUCATI"))

  checkEquals(1,
              nrow(s)
              )
}

test.GetItemData <- function() {
  project.data <- GetProjectData(project.name)

  i0 <-     GetItemData(project.name=project.name, project.data=project.data, keys=c("ES","MOTO","DUCATI"), value="V1")
  i0.bis <- GetItemData(project.name=project.name, project.data=project.data, id=5, value="V1")

  checkEquals(
              i0,
              i0.bis
              )
  checkEquals(
              "2010-2",
              rownames(i0)
              )
  checkEquals(
              c(0),
              i0[1,]
              )
  
  i1 <- GetItemData(project.name=project.name, project.data=project.data, keys=c("IT","CAR",""), value="V1")
  checkEquals(
              c(644.6, 646, 868, 501.2, 620, 290.3, 560, 680, 624.6, 311, 820, 250.6, 640, 440.6, 4560, 660),
              i1$V1
              )
  checkEquals(
              c("2003-1","2003-2","2004-1","2004-2","2005-1","2005-2","2006-1","2006-2","2007-1","2007-2","2008-1","2008-2","2009-1","2009-2","2010-1","2010-2"),
              rownames(i1)
              )

  i2 <- GetItemData(project.name=project.name, project.data=project.data, keys=c("", "MOTO","DUCATI"), value="V1")
  checkEquals(
              c(33, 5, 44, 36, 80, 0, 56, 0, 80, 43, 22, 24, 53, 44),
              i2$V1
              )
  checkEquals(
              c("2001-2", "2002-2", "2003-1", "2003-2", "2004-1"),
              rownames(i2)[1:5]
              )
}

test.00.BuildFilterWithKeys <- function() {
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
  ## sql filter
  checkEquals(
              "KEY1='IT' and KEY2='' and KEY3='101'", 
               BuildFilterWithKeys( c("IT", NA, "101"), sep="=", collapse=" and ", na.rm=FALSE)
              ) 
}


test.00.BuildFullKey <- function() {
  project.config <- GetProjectConfig(project.name)
  
  checkEquals(
              c("IT","",""),
              BuildFullKey(c("IT"), project.config$keys)
              )

  checkEquals(
              c("IT","101",""),
              BuildFullKey(c("IT","101"), project.config$keys)
              )

  checkEquals(
              c("IT","101","AAA"),
              BuildFullKey(c("IT","101", "AAA"), project.config$keys, fill.with="")
              )

  checkEquals(
              c(NA,NA,NA),
              BuildFullKey(NULL, project.config$keys, fill.with=NA)
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
  
#  checkEquals(
#              c("KEY2", "KEY3"),
#              BuildKeyNames( c(NA,"CAR","FIAT"), na.rm=TRUE )
#              )
#  checkEquals(
#              c("KEY2", "KEY3"),
#              BuildKeyNames( c(NULL,"CAR","FIAT"), na.rm=TRUE )
#              )
  checkEquals(
              c("KEY2", "KEY3"),
              BuildKeyNames( c("","CAR","FIAT"), na.rm=TRUE )
              )
  checkEquals(
              c("KEY1", "KEY2", "KEY3"),
              BuildKeyNames( c("","CAR","FIAT"), na.rm=FALSE )
              )
  
}

test.00.BuildPeriodRange <- function() {
  checkEquals(
              c("2001-2","2001-3","2002-1","2002-2","2002-3"),
              BuildPeriodRange(c(2001,2), 3, 5)
              )
  checkEquals(
              c("2001-3","2002-1","2002-2","2002-3","2003-1"),
              BuildPeriodRange(c(2001,2), 3, 5, shift=1)
              )
}

test.0.ParamFunctions <- function() {
  
  param.string <- "n.ahead=8;range=c(-Inf,Inf);NA2value=0;n.min=10;try.models=c('mean','trend','lm','es','arima');logtransform=FALSE;stepwise=TRUE;formula.right.lm='S*trend+S*trend2';rule='BestAIC';rule.noMaxOver=2;negTo0=FALSE;toInteger=TRUE"
  
  param <- EvalParamString(param.string)

  checkEquals( 12, length(param))

  checkEquals(8, param$n.ahead)
  
  checkEquals(
              c("mean", "trend", "lm", "es", "arima"),
              param$try.models
              )

  checkEquals(
              param.string,
              BuildParamString(project.config$param)
              )

  param1 <- EvalParamString("try.models=c('es','mean')")
  param2 <- MergeParamWithDefault(project.config=project.config, param=param1)

  checkEquals(
              length(param2),
              length(project.config$param)
              )
  checkEquals(
              c("es", "mean"),
              param2$try.models
              )

}            
              
test.00.GetDBTableName <- function() {
  checkEquals("sample_V1_summary",
              GetDBTableNameItemSummary("sample", value="V1")
              )
  checkEquals("sample_V1_results",
              GetDBTableNameItemResults("sample", value="V1")
              )
  checkEquals("sample_items",
              GetDBTableNameProjectItems("sample")
              )
}
  
test.GetDBTableSize <- function() {
  tablename <- GetDBTableNameProjectItems("sample")
  checkEquals(20,
              GetDBTableSize(tablename, db.channel=db.channel)
              )
}

test.00.GetFields <- function() {
  checkEquals(
              c("KEY1", "KEY2"),
              .GetFields(c("KEY1", "KEY2", "V1", "PERIOD"), "KEY")
              )
}

test.00.GetFieldsId <- function() {
  checkEquals(
              c(1,2),
              .GetFieldsId(c("KEY1", "KEY2", "V1", "PERIOD"), "KEY")
              )
}

test.GetKeyNames <- function() {
  checkEquals(
              c("KEY1", "KEY2", "KEY3"),
              GetKeyNames(keys=project.config$keys)
              )
  checkEquals(
              GetKeyNames(project.config=project.config),
              GetKeyNames(keys=project.config$keys)
              )
  checkEquals(
              GetKeyNames(project.name=project.name),
              GetKeyNames(keys=project.config$keys)
              )
}

test.GetProjectTablenamesDB <- function() {
   checkEquals(
               c("sample_items", "sample_V1_results", "sample_V1_summary", "sample_V2_results", "sample_V2_summary"),
               GetProjectTablenamesDB(project.name=project.name, project.config=project.config)
               )
}

test.GetValueNames <- function() {
  checkEquals(
              c("V1", "V2"),
              GetValueNames(values=project.config$values)
              )
  checkEquals(
              GetValueNames(project.config=project.config),
              GetValueNames(values=project.config$values)
              )
  checkEquals(
              GetValueNames(project.name=project.name),
              GetValueNames(values=project.config$values)
              )
}

test.GetUniqueKeyValues <- function() {
  uk <- GetUniqueKeyValues(project.name=project.name, project.config=project.config)
  checkEquals(
              length(project.config$keys),
              length(uk)
              )
  checkEquals(
              c("IT", "DE", "ES", "FR", "UK"),
              as.vector(uk$KEY1)
              )
}

test.GetItemRelativePath <- function() {
  checkEquals("0/1",
              GetItemRelativePath(1)
              )
  checkEquals("0/1/V1",
              GetItemRelativePath(1, "V1")
              )
}


test.GetItemChildren <- function() {
  checkEquals(
              c(3,4),
              GetItemChildren(id=11, project.name=project.name)
              )
  checkEquals(
              c(),
              GetItemChildren(id=1, project.name=project.name)
              )
}

test.GetItemPath <- function() {
  checkEquals(
              paste(GetProjectPath(project.name), GetItemRelativePath(1, "V1"), sep="/"),
              GetItemPath(project.name, 1, "V1")
              )
  checkEquals(
              "/var/www/strategico/projects/sample/0/1/V1",
              GetItemPath(project.name, 1, "V1")
              )
}

test.GetItemUrl <- function() {
  checkEquals(
              paste(GetProjectUrl(project.name), GetItemRelativePath(1, "V1"), sep="/"),
              GetItemUrl(project.name, 1, "V1")
              )
  checkEquals(
              "http://localhost/strategico/projects/sample/0/1/V1",
              GetItemUrl(project.name, 1, "V1")
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

test.is.project <- function() {
  checkEquals(
              TRUE,
              is.project("sample")
              )
  checkEquals(
              FALSE,
              is.project("missingproject")
              )
}

test.00.PeriodStringToVector <- function() {
  checkEquals(c(2001, 1),
              PeriodStringToVector("2001-1")
              )
  checkEquals(c(1987, 12),
              PeriodStringToVector("1987-12")
              )
}
