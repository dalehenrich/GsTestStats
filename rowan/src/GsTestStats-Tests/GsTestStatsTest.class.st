Class {
	#name : 'GsTestStatsTest',
	#superclass : 'TestCase',
	#category : 'GsTestStats-Tests'
}

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

{ #category : 'helpers' }
GsTestStatsTest >> assert: aFixture matches: aString [
	"Assert that all characters of aFixture are also in aString (also respect the order)."

	| index |
	index := 1.
	aFixture
		do: [ :char | 
			index := aString indexOf: char startingAt: index.
			index = 0
				ifTrue: [ 
					TestResult failure
						signal: 'Cannot match: "' , aFixture , '" to: "' , aString , '"' ] ]
]

{ #category : 'tests' }
GsTestStatsTest >> testReportSuccess [
	| runner reporter stream result |
	runner := (GsTestStatsCITestRunner
		runClasses: {SCIPassingTests}
		named: 'Stdout-testReportSuccess')
		yourself.
	stream := WriteStream on: (String new: 1000).
	reporter := SCITestReporterStdout report: runner on: stream.
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
