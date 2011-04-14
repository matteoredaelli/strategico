
connector.importItemsData <- function(project.path) {
  fullname <- paste(project.path,"external_data.csv", sep="/")
  ImportItemsDataFromCSV(project.path, filename=fullname, KEY=c("KEY1","KEY2","KEY3"),timesKeys=c("PERIOD"), VALUE=c("VALUE1","VALUE2"))
}
