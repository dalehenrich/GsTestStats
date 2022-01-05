Class {
	#name : 'GsTestStatsTest',
	#superclass : 'TestCase',
	#category : 'GsTestStats-Tests'
}

{ #category : 'fixtures' }
GsTestStatsTest class >> baselineTestForComparison [
	^ '{
	"branch" : "",
	"commitSha" : "",
	"deprecationWarnings" : [ ],
	"gsVersion" : "3.7.0",
	"notes" : "",
	"properties" : { },
	"resultsSummary" : {
		"errors" : 3,
		"failures" : 3,
		"passed" : 3,
		"summary" : "9 Tests with 3 Failures, 3 Errors and 0 DeprecationWarnings in 0.00s",
		"tests" : 9,
		"time" : 0.0
	},
	"suiteName" : "TestSuiteSample-testForComparison",
	"testCases" : [
		{
			"className" : "SCITestsForComparison",
			"status" : "errors",
			"selector" : "testError_1",
			"time" : "0"
		},
		{
			"className" : "SCITestsForComparison",
			"status" : "errors",
			"selector" : "testError_2",
			"time" : "0"
		},
		{
			"className" : "SCITestsForComparison",
			"status" : "errors",
			"selector" : "testError_3",
			"time" : "0"
		},
		{
			"className" : "SCITestsForComparison",
			"status" : "failures",
			"selector" : "testFail_1",
			"time" : "0"
		},
		{
			"className" : "SCITestsForComparison",
			"status" : "failures",
			"selector" : "testFail_2",
			"time" : "0"
		},
		{
			"className" : "SCITestsForComparison",
			"status" : "failures",
			"selector" : "testFail_3",
			"time" : "0"
		},
		{
			"className" : "SCITestsForComparison",
			"status" : "passed",
			"selector" : "testPass_1",
			"time" : "0"
		},
		{
			"className" : "SCITestsForComparison",
			"status" : "passed",
			"selector" : "testPass_2",
			"time" : "0"
		},
		{
			"className" : "SCITestsForComparison",
			"status" : "passed",
			"selector" : "testPass_3",
			"time" : "0"
		}
	],
	"timeStamp" : "2021-12-02T15:13:56.999921-08:00"
}'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> canonicalGemStoneVersion [
	^ '3.7.0'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> canonicalTimeStamp [
	^ DateAndTime fromString: '2021-12-02T15:13:56.999921-08:00'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> compareJsonToAddedAndRemovedTestCases [
	^ 'Diffs for PASSES: tests: + now passing;  ++ new passing test; - removed test [EFP is indication of previous test status]
 -P	SCITestsForComparison debug: #testPass_2
 ++	SCITestsForComparison debug: #testPass_4
Diffs for FAILURES: tests: + now failing;  ++ new failing test; - removed test [EFP is indication of previous test status]
 -F	SCITestsForComparison debug: #testFail_2
 ++	SCITestsForComparison debug: #testFail_4
Diffs for ERRORS: tests: + now erroring;  ++ new erroring test; - removed test [EFP is indication of previous test status]
 -E	SCITestsForComparison debug: #testError_2
 ++	SCITestsForComparison debug: #testError_4
'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> compareJsonToPassErrorAndFailureChanges [
	^ 'Diffs for PASSES: tests: + now passing;  ++ new passing test; - removed test [EFP is indication of previous test status]
 +E	SCITestsForComparison debug: #testError_3
 +F	SCITestsForComparison debug: #testFail_3
Diffs for FAILURES: tests: + now failing;  ++ new failing test; - removed test [EFP is indication of previous test status]
 +E	SCITestsForComparison debug: #testError_2
 +P	SCITestsForComparison debug: #testPass_2
Diffs for ERRORS: tests: + now erroring;  ++ new erroring test; - removed test [EFP is indication of previous test status]
 +F	SCITestsForComparison debug: #testFail_2
 +P	SCITestsForComparison debug: #testPass_3
'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> compareJsonToSelf [
	^ 'Diffs for PASSES: tests: + now passing;  ++ new passing test; - removed test [EFP is indication of previous test status]
Diffs for FAILURES: tests: + now failing;  ++ new failing test; - removed test [EFP is indication of previous test status]
Diffs for ERRORS: tests: + now erroring;  ++ new erroring test; - removed test [EFP is indication of previous test status]
'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> failureFixture [
	"
		'/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/GsTestStats/ex.txt' 
			asFileReference writeStreamDo: [:strm | strm nextPutAll: self ]
	"
	^ '
[1m[34m########################################################################
[1m[34m# Stdout-testReportFailure                                             #
[1m[34m# 8 Tests with 3 Failures, 2 Errors and 1 DeprecationWarnings in 0.00s #
[1m[34m########################################################################
[0m(3 tests passed)
[1m
SCIExcludedTests
[0m[32m ✓[0m #testDeprecation (ms)[0m
[32m ✓[0m #testShouldPass (ms)[0m
[32m ✓[0m #testShouldPassUnexpectedly (ms)[0m

[1m[31m#########################
[1m[31m# 5 tests did not pass: #
[1m[31m#########################
[0m[1m
SCIExcludedTests
[0m[1m[33m ✗ #testAssertError (ms)[0m
[1m[0m[1m[31m ✗ #testError (ms)[0m
[1m[0m[1m[33m ✗ #testFailure (ms)[0m
[1m[0m[1m[31m ✗ #testShouldFail (ms)[0m
[1m[0m[1m[33m ✗ #testThisIsAVeryLongMethodNameThatProbablyNeedsToBeContractedInOrderToBeDisplayedCorrectlyInATravisLog (ms)[0m
[1m[0m
[1m[31m######################################
[1m[31m# 1 smalltalkCI Deprecation Warnings #
[1m[31m######################################
[0m - SCIExcludedTests>>testDeprecation (This is just a test)


[1m[31m  Executed 8 Tests with 3 Failures, 2 Errors and 1 DeprecationWarnings in 0.00s.[0m

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
		"errors" : 2,
		"failures" : 3,
		"passed" : 3,
		"summary" : "8 Tests with 3 Failures, 2 Errors and 1 DeprecationWarnings in 0.00s",
		"tests" : 8,
		"time" : 0.0
	},
	"suiteName" : "TestSuiteSample-testReportFailure",
	"testCases" : [
		{
			"className" : "SCIExcludedTests",
			"status" : "failures",
			"selector" : "testAssertError",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedTests",
			"status" : "passed",
			"selector" : "testDeprecation",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedTests",
			"status" : "errors",
			"selector" : "testError",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedTests",
			"status" : "failures",
			"selector" : "testFailure",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedTests",
			"status" : "errors",
			"selector" : "testShouldFail",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedTests",
			"status" : "passed",
			"selector" : "testShouldPass",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedTests",
			"status" : "passed",
			"selector" : "testShouldPassUnexpectedly",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedTests",
			"status" : "failures",
			"selector" : "testThisIsAVeryLongMethodNameThatProbablyNeedsToBeContractedInOrderToBeDisplayedCorrectlyInATravisLog",
			"time" : "0"
		}
	],
	"timeStamp" : "2021-12-02T15:13:56.999921-08:00"
}'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> notPassingFixture [
	"
		'/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/GsTestStats/ex.txt' 
			asFileReference writeStreamDo: [:strm | strm nextPutAll: self ]
	"
	^ '
[1m[34m########################################################################
[1m[34m# Stdout-testReportNotPassing                                          #
[1m[34m# 8 Tests with 3 Failures, 2 Errors and 1 DeprecationWarnings in 0.00s #
[1m[34m########################################################################
[0m
[1m[32m##################
[1m[32m# 3 tests passed #
[1m[32m##################
[0m
[1m[31m#########################
[1m[31m# 5 tests did not pass: #
[1m[31m#########################
[0m[1m
SCIExcludedTests
[0m[1m[33m ✗ #testAssertError (ms)[0m
[1m[0m[1m[31m ✗ #testError (ms)[0m
[1m[0m[1m[33m ✗ #testFailure (ms)[0m
[1m[0m[1m[31m ✗ #testShouldFail (ms)[0m
[1m[0m[1m[33m ✗ #testThisIsAVeryLongMethodNameThatProbablyNeedsToBeContractedInOrderToBeDisplayedCorrectlyInATravisLog (ms)[0m
[1m[0m
[1m[31m######################################
[1m[31m# 1 smalltalkCI Deprecation Warnings #
[1m[31m######################################
[0m - SCIExcludedTests>>testDeprecation (This is just a test)


[1m[31m  Executed 8 Tests with 3 Failures, 2 Errors and 1 DeprecationWarnings in 0.00s.[0m

'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> successFixture [
	^ '
[1m[34m########################################################################
[1m[34m# Stdout-testReportSuccess                                             #
[1m[34m# 5 Tests with 0 Failures, 0 Errors and 0 DeprecationWarnings in 0.00s #
[1m[34m########################################################################
[0m(5 tests passed)
[1m
SCIPassingTests
[0m[32m ✓[0m #testAssert (ms)[0m
[32m ✓[0m #testAssertEquals (ms)[0m
[32m ✓[0m #testDeny (ms)[0m
[32m ✓[0m #testEmpty (ms)[0m
[32m ✓[0m #testRaise (ms)[0m


[1m[32m  Executed 5 Tests with 0 Failures, 0 Errors and 0 DeprecationWarnings in 0.00s.[0m

'
]

{ #category : 'fixtures' }
GsTestStatsTest class >> successJson [
	^ '{
	"branch" : "",
	"commitSha" : "",
	"deprecationWarnings" : [ ],
	"gsVersion" : "3.7.0",
	"notes" : "",
	"properties" : { },
	"resultsSummary" : {
		"errors" : 0,
		"failures" : 0,
		"passed" : 5,
		"summary" : "5 Tests with 0 Failures, 0 Errors and 0 DeprecationWarnings in 0.00s",
		"tests" : 5,
		"time" : 0.0
	},
	"suiteName" : "TestSuiteSample-testReportSuccess",
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
	"deprecationWarnings" : [ ],
	"gsVersion" : "3.7.0",
	"notes" : "",
	"properties" : { },
	"resultsSummary" : {
		"errors" : 0,
		"failures" : 0,
		"passed" : 5,
		"summary" : "5 Tests with 0 Failures, 0 Errors and 0 DeprecationWarnings in 0.00s",
		"tests" : 5,
		"time" : 0.0
	},
	"suiteName" : "TestSuiteSample-testReportSuccessWithBranch",
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
	"deprecationWarnings" : [ ],
	"gsVersion" : "3.7.0",
	"notes" : "",
	"properties" : { },
	"resultsSummary" : {
		"errors" : 0,
		"failures" : 0,
		"passed" : 5,
		"summary" : "5 Tests with 0 Failures, 0 Errors and 0 DeprecationWarnings in 0.00s",
		"tests" : 5,
		"time" : 0.0
	},
	"suiteName" : "TestSuiteSample-testReportSuccessWithBranchAndCommitSha",
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
GsTestStatsTest class >> unexpectedFailureJson [
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
		"errors" : 2,
		"failures" : 3,
		"passed" : 3,
		"summary" : "8 Tests with 3 Failures, 2 Errors and 1 DeprecationWarnings in 0.00s",
		"tests" : 8,
		"time" : 0.0
	},
	"suiteName" : "TestSuiteSample-testReportFailure",
	"testCases" : [
		{
			"className" : "SCIExcludedUnexpectedFailureTests",
			"status" : "failures",
			"selector" : "testAssertError",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedUnexpectedFailureTests",
			"status" : "passed",
			"selector" : "testDeprecation",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedUnexpectedFailureTests",
			"status" : "errors",
			"selector" : "testError",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedUnexpectedFailureTests",
			"status" : "failures",
			"selector" : "testFailure",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedUnexpectedFailureTests",
			"status" : "errors",
			"selector" : "testShouldFail",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedUnexpectedFailureTests",
			"status" : "passed",
			"selector" : "testShouldPass",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedUnexpectedFailureTests",
			"status" : "passed",
			"selector" : "testShouldPassUnexpectedly",
			"time" : "0"
		},
		{
			"className" : "SCIExcludedUnexpectedFailureTests",
			"status" : "failures",
			"selector" : "testThisIsAVeryLongMethodNameThatProbablyNeedsToBeContractedInOrderToBeDisplayedCorrectlyInATravisLog",
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
	aFixture = aString
		ifTrue: [ ^ self ].
	"attempt to isolate thelocation of the difference to make debugging easier"
	index := 1.
	aFixture
		do: [ :char | 
			| prevIndex |
			prevIndex := index.
			index := aString indexOf: char startingAt: index.
			index = 0
				ifTrue: [ 
					TestResult failure
						signal: 'Cannot match: "' , aFixture , '" to: "' , aString , '"' ] ].
	"hmmm, the still don't match"
	TestResult failure
		signal: 'Strings do not match: "' , aFixture , '" to: "' , aString , '"'
]

{ #category : 'tests' }
GsTestStatsTest >> testAddedAndRemovedTestSampleComparison [
	"Add a new test case and remove an existing test case"

	| sample1 sample2 stream result sampleDict |
	sampleDict := STON fromStream: self class baselineTestForComparison readStream.
	sample1 := GsTestSuiteSample fromDictionary: sampleDict.
	sample2 := sample1 copy.

	{#'testError_2'.
	#'testPass_2'.
	#'testFail_2'}
		do: [ :selector | 
			| testCase |
			testCase := sample2 testCases detect: [ :each | each selector == selector ].
			sample2 testCases remove: testCase ].

	sample2
		addTestCaseSample:
				(GsTestCaseSample new
						className: 'SCITestsForComparison';
						status: GsTestCaseSample passedStatus;
						selector: 'testPass_4';
						suite: sample2;
						time: 0;
						yourself);
		addTestCaseSample:
				(GsTestCaseSample new
						className: 'SCITestsForComparison';
						status: GsTestCaseSample failedStatus;
						selector: 'testFail_4';
						suite: sample2;
						time: 0;
						yourself);
		addTestCaseSample:
				(GsTestCaseSample new
						className: 'SCITestsForComparison';
						status: GsTestCaseSample erroredStatus;
						selector: 'testError_4';
						suite: sample2;
						time: 0;
						yourself);
		yourself.

	stream := WriteStream on: String new.
	sample2 compareTo: sample1 on: stream.
	result := stream contents.

	self assert: self class compareJsonToAddedAndRemovedTestCases matches: result
]

{ #category : 'tests' }
GsTestStatsTest >> testDebugUnexpectedTestFailure [
	| warnings expectedFailuresTestSuiteSample sawError |
	warnings := GsTestStatsCI deprecationWarnings.
	[ 
	GsTestStatsCI deprecatedReset.
	expectedFailuresTestSuiteSample := GsTestSuiteSample
		fromJsonStream: self class unexpectedFailureJson readStream.
	sawError := false.
	[ 
	GsTestStatsCITestRunner
		debugUnexpectedFailures: SCIExcludedUnexpectedFailureTests suite
		expectedFailures: expectedFailuresTestSuiteSample ]
		on: Error
		do: [ :ex | 
			self
				assert: ex messageText
				equals: 'SCIExcludedUnexpectedFailureTests debug: #testShouldPass'.
			sawError := true ].
	self assert: sawError description: 'expected a test error' ]
		ensure: [ GsTestStatsCI deprecationWarnings: warnings ]
]

{ #category : 'tests' }
GsTestStatsTest >> testDiferentTestSampleComparison [
	"copy a sample and change a passing test to failing, a passing test to erroring, a failing test to passing and an erroring test to failing"

	| sample1 sample2 stream result sampleDict |
	sampleDict := STON fromStream: self class baselineTestForComparison readStream.
	sample1 := GsTestSuiteSample fromDictionary: sampleDict.
	sample2 := sample1 copy.

	(sample2 testCases detect: [ :each | each selector == #'testError_2' ])
		setStatusFailed.
	(sample2 testCases detect: [ :each | each selector == #'testError_3' ])
		setStatusPassed.
	(sample2 testCases detect: [ :each | each selector == #'testFail_2' ])
		setStatusErrored.
	(sample2 testCases detect: [ :each | each selector == #'testFail_3' ])
		setStatusPassed.
	(sample2 testCases detect: [ :each | each selector == #'testPass_2' ])
		setStatusFailed.
	(sample2 testCases detect: [ :each | each selector == #'testPass_3' ])
		setStatusErrored.
"ADD A NEW TESTS AND REMOVE A TEST"
	stream := WriteStream on: String new.
	sample2 compareTo: sample1 on: stream.
	result := stream contents.

	self assert: self class compareJsonToPassErrorAndFailureChanges matches: result
]

{ #category : 'tests' }
GsTestStatsTest >> testEquivalentTestSampleComparison [
	| sample1 stream result sampleDict |
	sampleDict := STON fromStream: self class baselineTestForComparison readStream.
	sample1 := GsTestSuiteSample fromDictionary: sampleDict.

	stream := WriteStream on: String new.
	sample1 compareTo: sample1 on: stream.
	result := stream contents.

	self assert: self class compareJsonToSelf matches: result
]

{ #category : 'tests' }
GsTestStatsTest >> testStdoutReportNotPassing [
	| warnings isColorful runner reporter stream result |
	warnings := GsTestStatsCI deprecationWarnings.
	isColorful := GsTestStatsCI isColorful.
	[ 
	GsTestStatsCI isColorful: true.	"force isColorful to true"
	GsTestStatsCI deprecatedReset.
	runner := GsTestStatsCITestRunner
		runClasses: {SCIExcludedTests}
		named: 'Stdout-testReportNotPassing'.
	stream := WriteStream on: String new.
	reporter := GsTestStatsCITestReporterStdout reportNotPassing: runner on: stream.
	result := stream contents.
	self assert: self class notPassingFixture matches: result ]
		ensure: [ 
			GsTestStatsCI deprecationWarnings: warnings.
			GsTestStatsCI isColorful: isColorful ]
]

{ #category : 'tests' }
GsTestStatsTest >> testStdoutReportSuccess [
	| isColorful runner reporter stream result |
	isColorful := GsTestStatsCI isColorful.
	[ 
	GsTestStatsCI isColorful: true.	"force isColorful to true"
	runner := GsTestStatsCITestRunner
		runClasses: {SCIPassingTests}
		named: 'Stdout-testReportSuccess'.
	stream := WriteStream on: (String new: 1000).
	reporter := GsTestStatsCITestReporterStdout report: runner on: stream.
	result := stream contents.
	self assert: self class successFixture matches: result ]
		ensure: [ GsTestStatsCI isColorful: isColorful ]
]

{ #category : 'tests' }
GsTestStatsTest >> testStdoutTestFailure [
	| isColorful warnings runner reporter stream result |
	isColorful := GsTestStatsCI isColorful.
	warnings := GsTestStatsCI deprecationWarnings.
	[ 
	GsTestStatsCI isColorful: true.	"force isColorful to true"
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
		ensure: [ 
			GsTestStatsCI deprecationWarnings: warnings.
			GsTestStatsCI isColorful: isColorful ]
]

{ #category : 'tests' }
GsTestStatsTest >> testTestSuiteSampleReportSuccess [
	| runner reporter stream |
	runner := GsTestStatsCITestRunner
		runClasses: {SCIPassingTests}
		named: 'TestSuiteSample-testReportSuccess'.
	reporter := GsTestStatsCITestReporterTestSuiteSample report: runner.
	stream := WriteStream on: String new.
	reporter testSuiteSample timeStamp: self class canonicalTimeStamp.
	reporter testSuiteSample gsVersion: self class canonicalGemStoneVersion.
	reporter testSuiteSample exportJsonTo: stream.
	self assert: self class successJson matches: stream contents
]

{ #category : 'tests' }
GsTestStatsTest >> testTestSuiteSampleReportSuccessWithBranch [
	| runner reporter stream |
	runner := GsTestStatsCITestRunner
		runClasses: {SCIPassingTests}
		named: 'TestSuiteSample-testReportSuccessWithBranch'.
	reporter := GsTestStatsCITestReporterTestSuiteSample
		report: runner
		branch: 'master'.
	stream := WriteStream on: String new.
	reporter testSuiteSample timeStamp: self class canonicalTimeStamp.
	reporter testSuiteSample gsVersion: self class canonicalGemStoneVersion.
	reporter testSuiteSample exportJsonTo: stream.
	self assert: self class successJsonWithBranch matches: stream contents
]

{ #category : 'tests' }
GsTestStatsTest >> testTestSuiteSampleReportSuccessWithBranchAndCommitSha [
	| runner reporter stream |
	runner := GsTestStatsCITestRunner
		runClasses: {SCIPassingTests}
		named: 'TestSuiteSample-testReportSuccessWithBranchAndCommitSha'.
	reporter := GsTestStatsCITestReporterTestSuiteSample
		report: runner
		branch: 'v1'
		commitSha: 'd736a2425'.
	stream := WriteStream on: String new.
	reporter testSuiteSample timeStamp: self class canonicalTimeStamp.
	reporter testSuiteSample gsVersion: self class canonicalGemStoneVersion.
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
		named: 'TestSuiteSample-testReportFailure')
		failOnSCIDeprecationWarnings: true;
		yourself.
	reporter := GsTestStatsCITestReporterTestSuiteSample report: runner.
	stream := WriteStream on: String new.
	reporter testSuiteSample timeStamp: self class canonicalTimeStamp.
	reporter testSuiteSample gsVersion: self class canonicalGemStoneVersion.
	reporter testSuiteSample exportJsonTo: stream.
	self assert: self class failureJson matches: stream contents ]
		ensure: [ GsTestStatsCI deprecationWarnings: warnings ]
]

{ #category : 'tests' }
GsTestStatsTest >> testTestSuiteSampleTestForComparison [
	| warnings runner reporter stream |
	warnings := GsTestStatsCI deprecationWarnings.
	[ 
	GsTestStatsCI deprecatedReset.
	runner := (GsTestStatsCITestRunner
		runClasses: {SCITestsForComparison}
		named: 'TestSuiteSample-testForComparison')
		failOnSCIDeprecationWarnings: true;
		yourself.
	reporter := GsTestStatsCITestReporterTestSuiteSample report: runner.
	stream := WriteStream on: String new.
	reporter testSuiteSample timeStamp: self class canonicalTimeStamp.
	reporter testSuiteSample gsVersion: self class canonicalGemStoneVersion.
	reporter testSuiteSample exportJsonTo: stream.
	self assert: self class baselineTestForComparison matches: stream contents ]
		ensure: [ GsTestStatsCI deprecationWarnings: warnings ]
]
