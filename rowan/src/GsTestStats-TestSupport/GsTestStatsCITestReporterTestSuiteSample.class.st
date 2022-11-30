Class {
	#name : 'GsTestStatsCITestReporterTestSuiteSample',
	#superclass : 'GsTestStatsTestReporter',
	#instVars : [
		'testSuiteSample'
	],
	#category : 'GsTestStats-TestSupport'
}

{ #category : 'executing' }
GsTestStatsCITestReporterTestSuiteSample class >> report: aSCITestRunner [
	^ self new
		runner: aSCITestRunner;
		report;
		yourself
]

{ #category : 'executing' }
GsTestStatsCITestReporterTestSuiteSample class >> report: aSCITestRunner branch: branch [
	^ self new
		runner: aSCITestRunner;
		branch: branch commitSha: '';
		report;
		yourself
]

{ #category : 'executing' }
GsTestStatsCITestReporterTestSuiteSample class >> report: aSCITestRunner branch: branch commitSha: commitSha [
	^ self new
		runner: aSCITestRunner;
		branch: branch commitSha: commitSha;
		report;
		yourself
]

{ #category : 'accessing' }
GsTestStatsCITestReporterTestSuiteSample >> branch: branch commitSha: commitSha [
	self testSuiteSample
		branch: branch;
		commitSha: commitSha;
		yourself
]

{ #category : 'printing' }
GsTestStatsCITestReporterTestSuiteSample >> printDeprecationWarnings [
	self testSuiteSample deprecationWarnings: GsTestStatsCI deprecationWarnings
]

{ #category : 'printing' }
GsTestStatsCITestReporterTestSuiteSample >> printEnd [
]

{ #category : 'printing' }
GsTestStatsCITestReporterTestSuiteSample >> printNotPassingResults [
	runner results
		keysAndValuesDo: [ :class :results | 
			| className |
			className := class name asString.
			(results select: [ :result | result errored ])
				do: [ :erorredTestRunnerResult | 
					self testSuiteSample
						addTestCaseSample:
							(GsTestCaseSample new
								className: className;
								selector: erorredTestRunnerResult testCase selector;
								setStatusErrored;
								yourself) ].
			(results select: [ :result | result failed ])
				do: [ :failedTestRunnerResult | 
					self testSuiteSample
						addTestCaseSample:
							(GsTestCaseSample new
								className: className;
								selector: failedTestRunnerResult testCase selector;
								setStatusFailed;
								yourself) ] ]
]

{ #category : 'printing' }
GsTestStatsCITestReporterTestSuiteSample >> printPassingResults [
	runner results
		keysAndValuesDo: [ :class :results | 
			| className |
			className := class name asString.
			(results select: [ :result | result passed ])
				do: [ :passingTestRunnerResult | 
					self testSuiteSample
						addTestCaseSample:
							(GsTestCaseSample new
								className: className;
								selector: passingTestRunnerResult testCase selector;
								setStatusPassed;
								yourself) ] ]
]

{ #category : 'printing' }
GsTestStatsCITestReporterTestSuiteSample >> printResult: aResult [
	| testCase status |
	status := aResult passed
		ifTrue: [ 'passed' ]
		ifFalse: [ 
			aResult errored
				ifTrue: [ 'errors' ]
				ifFalse: [ 'failures' ] ].
	testCase := GsTestCaseSample new
		className: aResult testCase class name asString;
		selector: aResult testCase selector;
		status: status;
		suite: self testSuiteSample;
		time: aResult time;	"milliseconds"
		yourself.
	self testSuiteSample testCases add: testCase
]

{ #category : 'printing' }
GsTestStatsCITestReporterTestSuiteSample >> printStart [
	| dictClass |
	dictClass := (System myUserProfile objectNamed: #'GsTonelOrderedDictionary') ifNil: [ Dictionary ].
	self testSuiteSample
		suiteName: runner suiteName;
		timeStamp: DateAndTime now printString;
		properties: Dictionary new;
		notes: '';
		gsVersion: (System gemVersionAt: #'gsVersion');
		testCases: {};
		resultsSummary:
				(dictClass new
						at: #'errors' put: runner erroredTests;
						at: #'failures' put: runner failedTests;
						at: #'passed' put: runner passingTests;
						at: #'summary' put: runner summary;
						at: #'tests' put: runner totalTests;
						at: #'time' put: runner suiteTime / 1000.0;
						yourself).
]

{ #category : 'accessing' }
GsTestStatsCITestReporterTestSuiteSample >> testSuiteSample [
	^testSuiteSample ifNil: [testSuiteSample := GsTestSuiteSample new]
]
