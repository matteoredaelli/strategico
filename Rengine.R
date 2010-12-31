## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Authors: L. Finos, M. Redaelli

project.get_config <- function(project.config.fileName="project.config") { #cerca il file nella cartella : getwd()
  conf=read.table(project.config.fileName, head=FALSE,sep=":",stringsAsFactors =FALSE,quote="\"")
                                        #e assegnazione dei valori indicati dal file ai parametri
  project.name <- conf$V2[conf$V1=="project.name"]
  connector.package <- conf$V2[conf$V1=="connector.package"]
  eval.package <- conf$V2[conf$V1=="eval.package"]
  eval(parse(text=paste("save=c(",conf$V2[conf$V1=="save"],")"),))
  
  keys <- conf$V2[.get_fields.id(conf$V1,"key")]
  names(keys) <- .get_fields(conf$V1,"key")
  
  values <- conf$V2[.get_fields.id(conf$V1,"value")]
  names(values) <- .get_fields(conf$V1,"value")
  connector.package <- conf$V2[conf$V1=="connector.package"]
  
  period.freq <- as.numeric(conf$V2[.get_fields.id(conf$V1,"period.freq")])
  period.start <- as.numeric(strsplit(conf$V2[.get_fields.id(conf$V1,"period.start")],"-")[[1]] )
  period.end <- as.numeric(strsplit(conf$V2[.get_fields.id(conf$V1,"period.end")],"-")[[1]] )
  
  conf = conf[ .get_fields.id(conf$V1,"eval.param"),"V2",drop=FALSE] 
  
  CONFIG=list(project.name = project.name,
    connector.package=connector.package,
    eval.package=eval.package,
    keys = keys, 
    values = values ,
    connector.package = connector.package,
    period.freq = period.freq,
    period.start = period.start,
    period.end = period.end,
    save=save)
  for (i in 1:nrow(conf))
    eval(parse(text=paste("CONFIG$param$",conf[i,]),))
  
  source(CONFIG$connector.package)		
  source(CONFIG$eval.package)
  
  CONFIG
}

project.get_items <- function(project.path) {
  items.rdata <- paste( project.path, "items.Rdata", sep="/")
  load(items.rdata)
  Items
}

project.get_item_data <- function(project.path, keys) {
  load( paste(.get_item_path(keys,project.path), "item.Rdata", sep="/"))
  item_data
}

project.eval_item <- function(project.path, keys=NULL, pathToItem=NULL, values = NULL, param=NULL) {
  if(!exists("CONFIG")) assign("CONFIG", project.get_config(paste(project.path, "project.config", sep="/")), envir = .GlobalEnv)

  if(!is.null(pathToItem)) keys=strsplit(pathToItem,"/")[[1]]

  item.data <- project.get_item_data(project.path, keys)

  if (is.null(values)) values <- names(CONFIG$values)

  if(!is.null(keys)) print( paste(" Loading item: ", .get_item_name(keys) , sep=""))
  for (i in 1:length(values)) {
    print( paste(" Evaluating ", values[i],": ", CONFIG$values[values[i]], sep=""))  
    eval_item_by_value(project.path, keys, item.data, value=values[i],param=param)
  }
}


##trova un pattern in una lista di stringhe. utile per es per individuare le key e i value
##restituisce la stringa
.get_fields <- function(fields,pattern) {
  grep(paste("^",toupper(pattern),"[:digit:]*",sep=""), toupper(fields),value=TRUE)
}

##restituisce l'id
.get_fields.id <- function(fields,pattern) {
  grep(paste("^",toupper(pattern),"[:digit:]*",sep=""), toupper(fields))
}


.safe_name <- function(String) {
  gsub("[ '/\"\\:-<>]+", "_", String)
}

.get_item_name <- function( keys ) { 
  new_keys <- sapply( keys, .safe_name)
  paste( new_keys, collapse="-")
}

.get_item_path <- function( keys ,project.path=NULL,extra=NULL) {  
  new_keys <- sapply( keys[!is.na(keys)], .safe_name)
  subpath <- paste( new_keys, collapse="/")
  if (!is.null(project.path)) subpath <- paste(project.path, subpath, sep="/")
  if (!is.null(extra))  subpath <- paste(subpath,.safe_name(extra), sep="/")
  subpath
}

.stats.records <- function(filename, records, key, title="item records", top=25) {
  library(lattice)
  bitmap(filename)
  ## retriving top values
  ##data <- rev(sort(table(records[key])))
  data <-table(records[key])
  ##limit <- min(top, length(data))
  ##data <- data[1:limit]
  ## sorting by names
  ## data <- data[order(names(data))],
  img <- barchart(data,
                  scales = list(x=list(rot=90)),
                  horizontal=FALSE,
                  main=title,
                  col="green"
                  )
  print(img)
  dev.off()
}

.project.update_items_data_recursively <- function(projectPath, data, keys, values=NULL, csv=FALSE, stats=FALSE) {
  if (is.null(values))
    folder <- projectPath
  else
    folder <- paste(projectPath, paste(values,collapse="/"), sep="/")

  print(folder)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  ##print( paste("Folder=", folder, "Key=", key) )
  vals.names <- .get_fields(names(data),"value")
  temp=by(data[,vals.names],data$PERIOD, function(x) apply(x,2, sum, na.rm=TRUE))
  item_data <- as.data.frame(matrix(unlist(temp),ncol=length(vals.names)))
  rownames(item_data) <- names(temp)[!sapply(temp,is.null)]
  colnames(item_data) <- vals.names
  save(item_data, file= paste(folder, "item.Rdata", sep="/"))
  
  if (csv)
    write.csv(item_data,
              file= paste(folder, "item.csv", sep="/"),
              row.names = FALSE
              )
  if (stats)
    .stats.records(
                   paste(folder, "item.png", sep="/"),
                   item_data,
                   key)
  
  if (length(keys) > 0) {
    key <- keys[1]
    newKeys <- keys[-1]
    keyValues <- levels(factor(data[,key]))   ###CHANGED
    for (keyValue in keyValues) {
      ##	   print(keyValue)
      newValues = values
      newValues[key] = (keyValue)
      ##	    print( values[1,])
      
      newData <- data[data[,key]==keyValue,]
      
      .project.update_items_data_recursively(projectPath=projectPath, data=newData, keys=newKeys, values=newValues)
    }
  }
}


###########################aggiornamento dati - crea items.Rdata e item.RData
project.update_items_data <- function(projectPath, projectData, csv=FALSE) {

  ## estrai/filtra la lista degli item e li salva nel file items.Rdata

  key_fields <- .get_fields( colnames(projectData) ,"key" )
  
  projectData$PERIOD <- factor(projectData$PERIOD)
  for (i in key_fields){
    projectData[,i] <- factor(projectData[,i])
    levels(projectData[,i]) <- .safe_name(levels(projectData[,i]))
  }
  
  leaves <- unique(subset(projectData, select=key_fields) )
  outfile <- paste(projectPath, "/items.Rdata", sep="") 
  
  Items=leaves
  for (i in (ncol(leaves)):2){
    leaves[,i]=NA
    leaves= unique(leaves)
    Items=rbind(Items,unique(leaves))
  }
  save( Items, file=outfile)

  if (CSV)
    write.csv(Items,
              file= paste(projectPath, "/items.csv", sep=""),
              row.names = FALSE
              )	
  print(key_fields)			
  .project.update_items_data_recursively(projectPath, projectData, keys=key_fields, values=NULL )
  
} # end function


##input  da db. 
project.import_items_data.db <- function(projectPath, DB, DBUSER, DBPWD, sql_statement ) {
  
  library(RODBC)

  channel <- odbcConnect(DB, DBUSER, DBPWD)
  result <- sqlQuery(channel, sql_statement)
  odbcClose(channel)

  project.update_items_data(projectPath, result)
}

##input da da csv. 
project.import_items_data.csv <- function(projectPath, filename=NULL, KEY=c("KEY1","KEY2"), timesKeys=c("YEAR","SEMESTER"), VALUE=c("CORP")){ 

  ##restituisce una list (itemList) con una ts per ogni elemento. 
  ##names(itemList) è una parola composta dai valori assunti nei campi indicati da keys. separatore "[" 
  ##torna utile in seguito, nelle creazioni degli output dell'analisi
  
  if (is.null(filename)) filename=file.choose()
  data=read.csv(filename,sep=",") 

  names.data <- names(data)
  names(names.data) <- names(data)

  names.data[KEY]=paste(KEY,1:length(KEY),sep="")
  names.data[VALUE]=paste(VALUE,1:length(VALUE),sep="")

  names(data) = names.data
  data$PERIOD=.get_item_name(data[,timesKeys])

  project.update_items_data(projectPath, data[,c(KEY,"PERIOD",VALUE)])
}
