# Introduction #

When you create a new project (ex. myproject), you should create a file **myproject-connector.R** and add there a function **connector.importItemsData(project.path)** that gets the external data into a R dataframe with the following colnames:

  * KEY1
  * KEY2
  * ..
  * KEYN
  * PERIOD
  * VALUE1
  * VALUE2
  * ..
  * VALUEN

Notes:
  * It means you should put all time series into a single data frame.
  * "the primary key": there cannot be two rows with the same values of KEY1,KEY2,...,KEYN,PERIOD

Next step is to send teh new dataframe to one of the default functions for importing data provided by the engine:

  * ImportItemsDataFromDB
  * ImportItemsDataFromCSV