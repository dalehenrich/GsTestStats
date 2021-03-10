
```
bin/save.doit rowan_test_stats --branch=master examples/2021_03_05_3.7.0_testResults.json
bin/save.doit rowan_test_stats --branch=master examples/2021_03_03_3.5.0_testResults.json

bin/compare.doit rowan_test_stats --branch=master --version=3.7.0 examples/2021_03_03_3.5.0_testResults.json

bin/summary.doit rowan_test_stats --branch=master --version=3.7.0
bin/summary.doit rowan_test_stats --branch=master --version=3.5.0
```
