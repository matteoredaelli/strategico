# Sample #
  * project.name:sample
  * connector.package:sample\_connector.r
  * eval.package:eval\_ltp.r
  * period.start:1993-1
  * period.end:2010-1
  * period.freq:2
  * key1:Country
  * key2:Category
  * key3:Manufacturer
  * save:'summary','Rdata','report','t\_csv'
  * value1:MySales
  * value2:CompetitorsSales
  * eval.param01:XXX
  * ...

## save ##

  * images: generates charts, html page, ...
  * report: generates charts, html page, ...
  * csv: save imported item data to csv, save only predicted data in cols
  * db: save historical and predicted data to DB
  * t\_csv: save only predicted data in cols
  * fullcsv: save historical and predicted data in rows
  * summary\_db
  * summary\_csv