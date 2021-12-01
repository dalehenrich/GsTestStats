# GsTestStats


BRANCH | STATUS
------------- | -------------
**v1** | [![**v1** build status](https://github.com/dalehenrich/GsTestStats/actions/workflows/ci.yml/badge.svg?branch=v1)](https://github.com/dalehenrich/GsTestStats/actions)


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

The GsTestStats-TestSupport classes were derived from the SCITestRunner and SCITestReporter from [hpi-swa/smalltalkCI project](https://github.com/hpi-swa/smalltalkCI).

## Branch conventions
1. vX
2. vX.Y
3. vX.Y.Z or vX.Y.Z-id

### vX
Production branch.

X is incremented whenever there is a breaking change.
vX.Y and vX.Y.Z branches are merged into the VX branch, when development is complete on the feature or patch.

### vX.Y
Feature/Bug candidate branch.
 
Y is incremented whenever work on a new feature or bugfix is started.
vX.Y branches are merged into the VX branch when development is complete.

Primary work takes place on a vX.Y.Z branch and the VX.Y.Z branch is merged into the VX.Y branch at stable points, so if you want to have early access to a feature or bugfix, it is relatively safe to use this branch in production.

### vX.Y.Z
Development branch.

Z is incremented whenever work on a new feature or bugfix is started.
A pre-release may be used to further identify the purpose of the work.

Primary work takes place on this branch and cannot be depended upon to be usable.
