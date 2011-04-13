
test.BuildFilterWithKeys <- function() {
   checkEquals(
               "KEY1=='IT' & KEY2=='101'", 
               BuildFilterWithKeys( c("KEY1", "KEY2"), c("IT", "101"), sep="==", collapse=" & ")
               )
}


test.BuildSQLstmtDeleteRecordsWithKeys <- function() {
  checkEquals(
              "delete from europool_VALUE1  where KEY1='IT' and KEY2='101'",
              BuildSQLstmtDeleteRecordsWithKeys( "europool_VALUE1", c("KEY1", "KEY2"), c("IT", "101"))
              )
}

