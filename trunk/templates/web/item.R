if (is.null(GET$KEY1)) {
  id <- ifelse(is.null(GET$id), 1, as.numeric(GET$id))
  item.keys <- Item.GetKeys(project.name=project.name, id=id, db.channel=db.channel)
} else {
  for (k in project.keys)
    GET[[k]]=ifelse(is.null(GET[[k]]), "", GET[[k]])
  item.keys <- as.character( unlist(GET[project.keys]))

  id.result <- Project.GetIDs(project.name=project.name, keys=item.keys, db.channel=db.channel)
  id <- as.numeric(id.result[1])
}
keys.string <- ifelse(is.null(item.keys),
                      "",
                      paste(item.keys, collapse="-"))

