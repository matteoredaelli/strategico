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

test.050.Subset <- function() {
  project.data <- Project.GetData(project.name)
  s = SubsetByKeys(project.data, keys=c("ES","MOTO","DUCATI"))

  checkEquals(1,
              nrow(s)
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
  project.config <- Project.GetConfig(project.name)
  
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
  checkEquals("sample_summary_V1",
              DB.GetTableNameSummary("sample", value="V1")
              )
  checkEquals("sample_results_V1",
              DB.GetTableNameResults("sample", value="V1")
              )
  checkEquals("sample_items",
              DB.GetTableNameProject.Items("sample")
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

test.00.Period.FromToString <- function() {
  checkEquals(c(2001, 1),
              Period.FromString("2001-1")
              )
  checkEquals(c(1987, 12),
              Period.FromString("1987-12")
              )

  checkEquals("1999-1",
              Period.ToString(c(1999,1))
              )
  checkEquals("2009-12",
              Period.ToString(c(2009, 12))
              )
  
  checkEquals("2009-9",
              Period.ToString( Period.FromString("2009-9") )
              )
}
