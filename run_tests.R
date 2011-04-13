library('RUnit')

source("engine.R")

test.suite <- defineTestSuite("StrategicoTestSuite",
                              dirs = "tests",
                              testFileRegexp = 'test_.+\\.R$',
                              testFuncRegexp = '^test\\.+'
                              )
 
test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)
