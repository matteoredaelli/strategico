Import the sample project with

```
 Rscript import_items.Rscript projects/sample/
```

and then run
```
 Rscript run_tests.R
```


AN example of output is
```
matteo@soweto:~/strategico$ Rscript run_tests.R
Loading required package: methods

Executing test function test.BuildFilterWithKeys  ...  done successfully.

Executing test function test.BuildKeyNames  ...  done successfully.

Executing test function test.EvalItemValue  ... [1] " Loading item: IT"
[1] "  Time series: length= 18"
[1] " Evaluating VALUE1: Venduto"
Loading required package: tseries
Loading required package: quadprog
Loading required package: zoo
Loading required package: fracdiff
This is forecast 2.11 
arimaId: estimation by ML
criterion computed using exact likelihood
  p d q P D Q np    loglik      BIC      AIC rankAIC
1 0 1 1 0 1 0  1 -126.2674 255.4251 254.5347       1
       2011-1 2011-2 2012-1 2012-2 2013-1 2013-2 2014-1 2014-2
VALUE1   5192   1345   5824   1977   6456   2609   7088   3241
[1] " Loading item: IT"
[1] "  Time series: length= 18"
[1] " Evaluating VALUE1: Venduto"
arimaId: estimation by ML
criterion computed using exact likelihood
  p d q P D Q np    loglik      BIC      AIC rankAIC
1 0 1 1 0 1 0  1 -126.2674 255.4251 254.5347       1
       2011-1 2011-2 2012-1 2012-2 2013-1 2013-2 2014-1 2014-2
VALUE1   5192   1345   5824   1977   6456   2609   7088   3241
[1] " Loading item: IT-CAR"
[1] "  Time series: length= 16"
[1] " Evaluating VALUE2: VendutoMercato"
arimaId: estimation by ML
criterion computed using exact likelihood
  p d q P D Q np    loglik      BIC      AIC rankAIC
1 0 1 0 0 1 1  1 -117.0322 236.8371 236.0645       1
       2011-1 2011-2 2012-1 2012-2 2013-1 2013-2 2014-1 2014-2
VALUE2   6847   7388   6895   7436   6942   7483   6990   7531
[1] " Loading item: IT-CAR"
[1] "  Time series: length= 16"
[1] " Evaluating VALUE2: VendutoMercato"
arimaId: estimation by ML
criterion computed using exact likelihood
  p d q P D Q np    loglik      BIC      AIC rankAIC
1 0 1 0 0 1 1  1 -117.0322 236.8371 236.0645       1
       2011-1 2011-2 2012-1 2012-2 2013-1 2013-2 2014-1 2014-2
VALUE2   6847   7388   6895   7436   6942   7483   6990   7531
 done successfully.

RUNIT TEST PROTOCOL -- Thu Apr 14 22:22:48 2011 
*********************************************** 
Number of test functions: 10 
Number of errors: 0 
Number of failures: 0 

1 Test Suite : 
StrategicoTestSuite - 10 test functions, 0 errors, 0 failures

Details 
*************************** 
Test Suite: StrategicoTestSuite 
Test function regexp: ^test\.+ 
Test file regexp: test_.+\.R$ 
Involved directory: 
tests 
--------------------------- 
Test file: tests/test_engine.R 
test.BuildFilterWithKeys: (6 checks) ... OK (0 seconds)
test.BuildKeyNames: (9 checks) ... OK (0 seconds)
test.BuildPeriodRange: (1 checks) ... OK (0 seconds)
test.BuildSQLstmtDeleteRecordsWithKeys: (2 checks) ... OK (0 seconds)
test.GetFields: (1 checks) ... OK (0 seconds)
test.GetFieldsId: (1 checks) ... OK (0 seconds)
test.GetProjectConfig: (7 checks) ... OK (0.03 seconds)
test.incSampleTime: (2 checks) ... OK (0 seconds)
test.ProjectData: (5 checks) ... OK (0.01 seconds)
--------------------------- 
Test file: tests/test_ltp.R 
test.EvalItemValue: (5 checks) ... OK (16.86 seconds)
```