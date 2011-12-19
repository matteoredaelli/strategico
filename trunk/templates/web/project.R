db.channel <- DB.Connect()
project.name <- ifelse(is.null(GET$project), "sample", GET$project)
project.name <- gsub("\\.", "", project.name)
project.name <- gsub(" ", "", project.name)

project.config <- Project.GetConfig(project.name=project.name, quit=FALSE)
if (!is.null(project.config)) {
  project.keys <- GetKeyNames(project.config$keys)
} else {
  project.keys <- c()
}

project.path <- Project.GetPath(project.name)
strategico.command <- paste(strategico.config$strategico.command, "-n", project.name)
