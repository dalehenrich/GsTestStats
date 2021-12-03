Class {
	#name : 'GsTestStatsTest',
	#superclass : 'TestCase',
	#category : 'GsTestStats-Tests'
}

{ #category : 'fixtures' }
GsTestStatsTest class >> canonicalTimeStamp [
	^ DateAndTime fromString: '2021-12-02T15:13:56.999921-08:00'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> failureFixture [
	^ '
[1m[34m#############################################
[1m[34m# Stdout-testReportFailure                  #
[1m[34m# 8 Tests with 4 Failures and 1 Errors in s #
[1m[34m#############################################
[0m
(3 tests passed)
[1m
SCIExcludedTests
[0m[32m ✓[0m #testDeprecation (ms)[0m
[32m ✓[0m #testShouldFail (ms)[0m
[32m ✓[0m #testShouldPass (ms)[0m
[1m[31m#########################
[1m[31m# 5 tests did not pass: #
[1m[31m#########################
[0m[1m
SCIExcludedTests
[0m[1m[33m ✗ #testAssertError (ms)[0m
[0m[1m[31m ✗ #testError (ms)[0m
[0m[1m[33m ✗ #testFailure (ms)[0m
[0m[1m[33m ✗ #testShouldPassUnexpectedly (ms)[0m
[0m[1m[33m ✗ #testThisIsAVeryLongMethodNameThat...playedCorrectlyInATravisLog (ms)[0m
[1m[31m###########
[1m[31m# Summary #
[1m[31m###########
[0m[1m
SCIExcludedTests
[0m[33m ✗ #testAssertError (ms)[0m
[31m ✗ #testError (ms)[0m
[33m ✗ #testFailure (ms)[0m
[33m ✗ #testShouldPassUnexpectedly (ms)[0m
[33m ✗ #testThisIsAVeryLongMethodNameThatProbablyNeedsToBeContractedInOrderToBeDisplayedCorrectlyInATravisLog (ms)[0m
[1m[31msmalltalkCI Deprecation Warnings[0m
 - SCIExcludedTests>>testDeprecation (This is just a test)
[1m[31m  Executed 8 Tests with 4 Failures and 1 Errors in s.[0m
'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> failureJson [
	^ '{
	"branch" : "",
	"commitSha" : "",
	"deprecationWarnings" : [
		"SCIExcludedTests>>testDeprecation (This is just a test)"
	],
	"gsVersion" : "3.7.0",
	"notes" : "",
	"properties" : { },
	"resultsSummary" : {
		"tests" : 8,
		"summary" : "8 Tests with 4 Failures and 1 Errors in 0.00s",
		"passed" : 3,
		"errors" : 1,
		"failures" : 4,
		"time" : 0.0
	},
	"suiteName" : "Stdout-testReportFailure",
	"testCases" : [
		{
			"className" : "SCIExcludedTests",
			"status" : "passed",
			"selector" : "testDeprecation",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedTests",
			"status" : "passed",
			"selector" : "testShouldFail",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedTests",
			"status" : "passed",
			"selector" : "testShouldPass",
			"time" : "0"
		}
	],
	"timeStamp" : "2021-12-02T15:13:56.999921-08:00"
}'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> successFixture [
	^ '
[1m[34m#############################################
[1m[34m# Stdout-testReportSuccess                  #
[1m[34m# 5 Tests with 0 Failures and 0 Errors in s #
[1m[34m#############################################
[0m
(5 tests passed)
[1m
SCIPassingTests
[0m[32m ✓[0m #testAssert (ms)[0m
[32m ✓[0m #testAssertEquals (ms)[0m
[32m ✓[0m #testDeny (ms)[0m
[32m ✓[0m #testEmpty (ms)[0m
[32m ✓[0m #testRaise (ms)[0m
[1m[32m  Executed 5 Tests with 0 Failures and 0 Errors in s.[0m
'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> successJson [
	^ '{
	"branch" : "",
	"commitSha" : "",
	"deprecationWarnings" : [
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)"
	],
	"gsVersion" : "3.7.0",
	"notes" : "",
	"properties" : { },
	"resultsSummary" : {
		"tests" : 5,
		"summary" : "5 Tests with 0 Failures and 0 Errors in 0.00s",
		"passed" : 5,
		"errors" : 0,
		"failures" : 0,
		"time" : 0.0
	},
	"suiteName" : "Stdout-testReportSuccess",
	"testCases" : [
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testAssert",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testAssertEquals",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testDeny",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testEmpty",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testRaise",
			"time" : "0"
		}
	],
	"timeStamp" : "2021-12-02T15:13:56.999921-08:00"
}'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> successJsonWithBranch [
	^ '{
	"branch" : "master",
	"commitSha" : "",
	"deprecationWarnings" : [
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)"
	],
	"gsVersion" : "3.7.0",
	"notes" : "",
	"properties" : { },
	"resultsSummary" : {
		"tests" : 5,
		"summary" : "5 Tests with 0 Failures and 0 Errors in 0.00s",
		"passed" : 5,
		"errors" : 0,
		"failures" : 0,
		"time" : 0.0
	},
	"suiteName" : "Stdout-testReportSuccess",
	"testCases" : [
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testAssert",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testAssertEquals",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testDeny",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testEmpty",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testRaise",
			"time" : "0"
		}
	],
	"timeStamp" : "2021-12-02T15:13:56.999921-08:00"
}'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> successJsonWithBranchAndCommitSha [
	^ '{
	"branch" : "v1",
	"commitSha" : "d736a2425",
	"deprecationWarnings" : [
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)",
		"SCIExcludedTests>>testDeprecation (This is just a test)"
	],
	"gsVersion" : "3.7.0",
	"notes" : "",
	"properties" : { },
	"resultsSummary" : {
		"tests" : 5,
		"summary" : "5 Tests with 0 Failures and 0 Errors in 0.00s",
		"passed" : 5,
		"errors" : 0,
		"failures" : 0,
		"time" : 0.0
	},
	"suiteName" : "Stdout-testReportSuccess",
	"testCases" : [
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testAssert",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testAssertEquals",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testDeny",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testEmpty",
			"time" : "0"
		},
		{
			"className" : "SCIPassingTests",
			"status" : "passed",
			"selector" : "testRaise",
			"time" : "0"
		}
	],
	"timeStamp" : "2021-12-02T15:13:56.999921-08:00"
}'
]

{ #category : 'helpers' }
GsTestStatsTest >> assert: aFixture matches: aString [
	"Assert that all characters of aFixture are also in aString (also respect the order)."

	| index |
	index := 1.
	aFixture
		do: [ :char | 
			| prevIndex |
			prevIndex := index.
			index := aString indexOf: char startingAt: index.
			index = 0
				ifTrue: [ 
					TestResult failure
						signal: 'Cannot match: "' , aFixture , '" to: "' , aString , '"' ] ]
]

{ #category : 'tests' }
GsTestStatsTest >> testStdoutReportSuccess [
	| runner reporter stream result |
	runner := GsTestStatsCITestRunner
		runClasses: {SCIPassingTests}
		named: 'Stdout-testReportSuccess'.
	stream := WriteStream on: (String new: 1000).
	reporter := GsTestStatsCITestReporterStdout report: runner on: stream.
	result := stream contents.
	self assert: self class successFixture matches: result
]

{ #category : 'tests' }
GsTestStatsTest >> testStdoutTestFailure [
	| warnings runner reporter stream result |
	warnings := GsTestStatsCI deprecationWarnings.
	[ 
	GsTestStatsCI deprecatedReset.
	runner := (GsTestStatsCITestRunner
		runClasses: {SCIExcludedTests}
		named: 'Stdout-testReportFailure')
		failOnSCIDeprecationWarnings: true;
		yourself.
	stream := WriteStream on: String new.
	reporter := GsTestStatsCITestReporterStdout report: runner on: stream.
	result := stream contents.
	self assert: self class failureFixture matches: result ]
		ensure: [ GsTestStatsCI deprecationWarnings: warnings ]
]

{ #category : 'tests' }
GsTestStatsTest >> testTestSuiteSampleReportSuccess [
	| runner reporter stream |
	runner := GsTestStatsCITestRunner
		runClasses: {SCIPassingTests}
		named: 'Stdout-testReportSuccess'.
	reporter := GsTestStatsCITestReporterTestSuiteSample report: runner.
	stream := WriteStream on: String new.
	reporter testSuiteSample timeStamp: self class canonicalTimeStamp.
	reporter testSuiteSample exportJsonTo: stream.
	self assert: self class successJson matches: stream contents
]

{ #category : 'tests' }
GsTestStatsTest >> testTestSuiteSampleReportSuccessWithBranch [
	| runner reporter stream |
	runner := GsTestStatsCITestRunner
		runClasses: {SCIPassingTests}
		named: 'Stdout-testReportSuccess'.
	reporter := GsTestStatsCITestReporterTestSuiteSample
		report: runner
		branch: 'master'.
	stream := WriteStream on: String new.
	reporter testSuiteSample timeStamp: self class canonicalTimeStamp.
	reporter testSuiteSample exportJsonTo: stream.
	self assert: self class successJsonWithBranch matches: stream contents
]

{ #category : 'tests' }
GsTestStatsTest >> testTestSuiteSampleReportSuccessWithBranchAndCommitSha [
	| runner reporter stream |
	runner := GsTestStatsCITestRunner
		runClasses: {SCIPassingTests}
		named: 'Stdout-testReportSuccess'.
	reporter := GsTestStatsCITestReporterTestSuiteSample
		report: runner
		branch: 'v1'
		commitSha: 'd736a2425'.
	stream := WriteStream on: String new.
	reporter testSuiteSample timeStamp: self class canonicalTimeStamp.
	reporter testSuiteSample exportJsonTo: stream.
	self
		assert: self class successJsonWithBranchAndCommitSha
		matches: stream contents
]

{ #category : 'tests' }
GsTestStatsTest >> testTestSuiteSampleTestFailure [
	| warnings runner reporter stream |
	warnings := GsTestStatsCI deprecationWarnings.
	[ 
	GsTestStatsCI deprecatedReset.
	runner := (GsTestStatsCITestRunner
		runClasses: {SCIExcludedTests}
		named: 'Stdout-testReportFailure')
		failOnSCIDeprecationWarnings: true;
		yourself.
	reporter := GsTestStatsCITestReporterTestSuiteSample report: runner.
	stream := WriteStream on: String new.
	reporter testSuiteSample timeStamp: self class canonicalTimeStamp.
	reporter testSuiteSample exportJsonTo: stream.
	self assert: self class failureJson matches: stream contents ]
		ensure: [ GsTestStatsCI deprecationWarnings: warnings ]
]
