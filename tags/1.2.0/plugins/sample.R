
sample.importItemsData <- function(project.name) {
  fullname <- paste("tests", "external_data.csv", sep="/")
  Project.ImportDataFromCSV(project.name, filename=fullname,
                           KEY=c("KEY1","KEY2","KEY3"),
                           timesKeys=c("PERIOD"),
                           V=c("V1","V2"))
}
