# GsTestStats

GemStone-based test result DB, where test suite results are recorded on a GemStone version and git branch basis. The primary use case is to identify the differences between test runs during intense development where there are a sufficient quatity of "expected" test failures to make keeping track manually problematic. Marking a test as expected failure is not sufficient because the "expected" test failures vary based on GemStone verson and git branch.

At the end of a test run, the current results are compared to the results from the previous run and a report like the following is produced
```
    Diffs for PASSES: tests: + now passing;  ++ new passing test; - removed test [EFP is indication of previous test status]
     +FRwProjectFiletreeReaderWriterTest>>testReadExistingDiskProjectWithEmptyClassExtension
     +FRwProjectTonelReaderWriterTest>>testReadExistingDiskProjectWithEmptyClassExtension
     ++RwRowanSample9Test>>testSpec_0064_1
     ++RwRowanSample9Test>>testSpec_0064_2
     ++RwRowanSample9Test>>testSpec_0064_3
     ++RwRowanSample9Test>>testSpec_0064_4
     ++RwRowanSample9Test>>testSpec_0064_5
    Diffs for FAILURES: tests: + now failing;  ++ new failing test; - removed test [EFP is indication of previous test status]
     +PRowanPackageServiceTest>>test_projectIsDirtyWithNonUpdatedPackage
     +PRowanPackageServiceTest>>test_testClassesIncludesExtensions
     +PRowanQueryServicesTest>>test_hierarchyImplementorsGetsAllSubclasses
     +PRwGsTopazRowanToolTest>>testTopazListProjects
    Diffs for ERRORS: tests: + now erroring;  ++ new erroring test; - removed test [EFP is indication of previous test status]
     +PRwRowanSample9Test>>testIssue460_0
    false
```

The code started life as part of the [st_launcher](https://github.com/dalehenrich/st_launcher) project. I am extracting the code from the the **st_launcher** project so that I can write scripts using [superDoit](https://github.com/dalehenrich/superDoit) ... Using tonel class files turns out to not be the best way to write scripts and I think that the **superDoit** project will be more flexible and easier to use ... 
