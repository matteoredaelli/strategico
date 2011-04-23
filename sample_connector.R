
connector.importItemsData <- function(project.name) {
  fullname <- paste("projects", project.name,"external_data.csv", sep="/")
  ImportProjectDataFromCSV(project.name, filename=fullname, KEY=c("KEY1","KEY2","KEY3"),timesKeys=c("PERIOD"), VALUE=c("VALUE1","VALUE2"))
}
