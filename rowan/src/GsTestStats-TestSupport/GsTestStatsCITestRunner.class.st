Class {
	#name : 'GsTestStatsCITestRunner',
	#superclass : 'Object',
	#instVars : [
		'suite',
		'suiteTime',
		'results',
		'properties'
	],
	#category : 'GsTestStats-TestSupport'
}

{ #category : 'running' }
GsTestStatsCITestRunner class >> debugClasses: aCollectionOfClasses named: aSuiteName [
	| suite classes |
	suite := TestSuite named: aSuiteName.
	classes := (aCollectionOfClasses
		select: [ :each | (each includesBehavior: TestCase) and: [ each isAbstract not ] ])
		asSortedCollection: [ :a :b | a name <= b name ].
	classes do: [ :each | each addToSuiteFromSelectors: suite ].
	^ self debugSuite: suite
]

{ #category : 'running' }
GsTestStatsCITestRunner class >> debugSuite: aTestSuite [
	^ self new
		initializeOn: aTestSuite;
		debug
]

{ #category : 'running' }
GsTestStatsCITestRunner class >> debugUnexpectedFailures: aTestSuite expectedFailures: aGsTestSuiteSample [
	^ GsTestStatsCIDebugUnexpectedFailuresTestRunner new
		initializeOn: aTestSuite;
		expectedFailuresTestSuiteSample: aGsTestSuiteSample;
		debug
]

{ #category : 'constants' }
GsTestStatsCITestRunner class >> errorExceptions [
	^ Halt, Error, TestFailure
]

{ #category : 'compatibility' }
GsTestStatsCITestRunner class >> isTestMethod: aMethod [
	"Compatibility function for older versions of Squeak that do not have CompiledMethod>>isTestMethod"
	^ (aMethod methodClass isKindOf: TestCase class)
		and: [ aMethod selector beginsWith: 'test' ]
]

{ #category : 'instance creation' }
GsTestStatsCITestRunner class >> new [
	^ self basicNew initialize
]

{ #category : 'running' }
GsTestStatsCITestRunner class >> runClasses: aCollectionOfClasses named: aSuiteName [
	| suite classes |
	suite := TestSuite named: aSuiteName.
	classes := (aCollectionOfClasses
		select: [ :each | (each includesBehavior: TestCase) and: [ each isAbstract not ] ])
		asSortedCollection: [ :a :b | a name <= b name ].
	classes do: [ :each | each addToSuiteFromSelectors: suite ].
	^ self runSuite: suite
]

{ #category : 'running' }
GsTestStatsCITestRunner class >> runSuite: aTestSuite [
	^ self new
		initializeOn: aTestSuite;
		run
]

{ #category : 'constants' }
GsTestStatsCITestRunner class >> skipExceptions [
	^ ExceptionSet new
]

{ #category : 'running' }
GsTestStatsCITestRunner >> basicRunCase: aTestCase [
	aTestCase runCase	
]

{ #category : 'running' }
GsTestStatsCITestRunner >> debug [
	[ 
	self setUp.
	suiteTime := GsTestStatsCI timeToRun: [ self debugAll ] ]
		ensure: [ self tearDown ]
]

{ #category : 'running' }
GsTestStatsCITestRunner >> debugAll [
	suite tests do: [ :each | each debug: self ]
]

{ #category : 'running' }
GsTestStatsCITestRunner >> debugCase: aTestCase [
	| result |
	GsTestStatsCI printDebug: aTestCase asString.
	result := GsTestStatsCITestRunnerResult new
		testCase: aTestCase;
		yourself.
	self basicRunCase: aTestCase.
	self printProgress: result.
	(results at: aTestCase class ifAbsentPut: [ OrderedCollection new ])
		add: result
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> erroredTests [
	| size |
	size := 0.
	results valuesDo: [ :clsResults |
		size := size + (clsResults select: [ :r | r errored ]) size ].
	^ size
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> failedTests [
	| size |
	size := 0.
	results valuesDo: [ :clsResults |
		size := size + (clsResults select: [ :r | r failed ]) size ].
	^ size
]

{ #category : 'properties' }
GsTestStatsCITestRunner >> failOnSCIDeprecationWarnings [
	^ properties at: #'failOnSCIDeprecationWarnings' ifAbsent: [ false ]
]

{ #category : 'properties' }
GsTestStatsCITestRunner >> failOnSCIDeprecationWarnings: object [
	properties at: #'failOnSCIDeprecationWarnings' put: object
]

{ #category : 'properties' }
GsTestStatsCITestRunner >> failOnZeroTests [
	^ properties at: #'failOnZeroTests' ifAbsent: [ false ]
]

{ #category : 'properties' }
GsTestStatsCITestRunner >> failOnZeroTests: object [
	properties at: #'failOnZeroTests' put: object
]

{ #category : 'properties' }
GsTestStatsCITestRunner >> hidePassingTests [
	^ properties at: #'hidePassingTests' ifAbsent: [ false ]
]

{ #category : 'properties' }
GsTestStatsCITestRunner >> hidePassingTests: object [
	^ properties at: #'hidePassingTests' put: object
]

{ #category : 'initialize-release' }
GsTestStatsCITestRunner >> initialize [
	super initialize.
	suiteTime := Duration seconds: 0.
	properties := Dictionary new.
	results := Dictionary new
]

{ #category : 'initialize-release' }
GsTestStatsCITestRunner >> initializeOn: aTestSuite [
	suite := aTestSuite
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> isSuccessful [
	| deprecationWarningCheck |
	deprecationWarningCheck := self failOnSCIDeprecationWarnings
		ifTrue: [ GsTestStatsCI deprecationWarnings isEmpty ]
		ifFalse: [ true ].
	^ deprecationWarningCheck and: [
			(self erroredTests + self failedTests) = 0]
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> passingTests [
	^ self totalTests - self erroredTests - self failedTests
]

{ #category : 'running' }
GsTestStatsCITestRunner >> printProgress: aTestResult [
	GsTestStatsCI stdout nextPut: (aTestResult passed
		ifTrue: [ $. ]
		ifFalse: [ (aTestResult failed)
			ifTrue: [ $F ]
			ifFalse: [ $E ] ])
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> properties [
	^properties
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> properties: object [
	properties := object
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> results [

	^ results
]

{ #category : 'running' }
GsTestStatsCITestRunner >> run [
	[ 
	self setUp.
	suiteTime := GsTestStatsCI timeToRun: [ self runAll ] ]
		ensure: [ self tearDown ]
]

{ #category : 'running' }
GsTestStatsCITestRunner >> runAll [
	suite tests do: [ :each | each run: self ]
]

{ #category : 'running' }
GsTestStatsCITestRunner >> runCase: aTestCase [
	| result |
	GsTestStatsCI printDebug: aTestCase asString.
	result := GsTestStatsCITestRunnerResult new
		testCase: aTestCase;
		yourself.
	result
		time:
			(GsTestStatsCI
				timeToRun: [ 
					[ 
					[ self basicRunCase: aTestCase ]
						on: self class errorExceptions
						do: [ :err | 
							result testError: err.
							aTestCase shouldPass & self serializeError
								ifTrue: [ result stack: (self stackTraceString: err of: aTestCase) ] ] ]
						on: self class skipExceptions
						do: [ :skip | result skipped: true ] ]).
	self printProgress: result.
	(results at: aTestCase class ifAbsentPut: [ OrderedCollection new ])
		add: result
]

{ #category : 'properties' }
GsTestStatsCITestRunner >> serializeError [
	^ properties at: #'serializeError' ifAbsent: [ false ]
]

{ #category : 'properties' }
GsTestStatsCITestRunner >> serializeError: object [
	properties at: #'serializeError' put: object
]

{ #category : 'private' }
GsTestStatsCITestRunner >> serializeError: error of: aTestCase [
	"Hook for supporting post-mortem stack dumps. Do nothing by default."
]

{ #category : 'running' }
GsTestStatsCITestRunner >> setUp [
	GsTestStatsCI printToStdout: 'Running suite "', self suiteName ,'" with ', suite tests size asString, ' tests.'.

	"Initialize the test resources."
	suite resources do: [ :each |
		each isAvailable
			ifFalse: [ TestResult signalErrorWith: 'Resource ' , each name , ' could not be initialized' ] ]
]

{ #category : 'private' }
GsTestStatsCITestRunner >> stackTraceString: err of: aTestCase [
	^ GsProcess stackReportToLevel: 100
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> suite [

	^ suite
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> suite: anObject [

	suite := anObject
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> suiteName [

	^ suite name
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> suiteTime [

	^ suiteTime
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> suiteTimeInSeconds [
	^ (self suiteTime / 1000.0)
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> suiteTimeString [
	^ (GsTestStatsCI stringFor: self suiteTimeInSeconds maxDecimalPlaces: 2), 's'
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> summary [
	^ self totalTests asString , ' Tests with ' , self failedTests asString
		, ' Failures, ' , self erroredTests asString , ' Errors and '
		, GsTestStatsCI deprecationWarnings size asString , ' DeprecationWarnings in '
		, self suiteTimeString
]

{ #category : 'running' }
GsTestStatsCITestRunner >> tearDown [
	suite resources
		do: [ :each | each reset ].

	GsTestStatsCI printToStdout: 'Finished running suite: ', self suiteName.
	
	(self failOnZeroTests and: [ self totalTests == 0 ])
		ifTrue: [ Error signal: 'No tests were executed.

If this is intended, use `#failOnZeroTests : false` in your SmalltalkCISpec.' ].
]

{ #category : 'accessing' }
GsTestStatsCITestRunner >> totalTests [
	| size |
	size := 0.
	results valuesDo: [ :clsResults |
		size := size + clsResults size ].
	^ size

]
