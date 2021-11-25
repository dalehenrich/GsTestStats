Class {
	#name : 'GsTestStatsTestReporter',
	#superclass : 'SCITestReporter',
	#instVars : [
		'testSuiteSample'
	],
	#category : 'GsTestStats-TestSupport'
}

{ #category : 'executing' }
GsTestStatsTestReporter class >> report: aSCITestRunner [
	^ self report: aSCITestRunner spec: SmalltalkCISpec new
]

{ #category : 'executing' }
GsTestStatsTestReporter class >> report: aSCITestRunner spec: aSCISpec [
	^ self new
		runner: aSCITestRunner;
		spec: aSCISpec;
		report
]

{ #category : 'executing' }
GsTestStatsTestReporter class >> report: aSCITestRunner spec: aSCISpec on: aStream [
	self shouldNotImplement: #'report:spec:on:'
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printEnd [
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printException: aResult [
	self shouldNotImplement: #'printException:'
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printResult: aResult [
	| testCase status |
	status := aResult passed
		ifTrue: [ 'passed' ]
		ifFalse: [ 
			aResult errored
				ifTrue: [ 'errors' ]
				ifFalse: [ 'failures' ] ].
	testCase := Dictionary new
		at: #'className' put: aResult testCase class name;
		at: #'selector' put: aResult testCase selector;
		at: #'time' put: aResult time / 1000.0;
		at: #'status' put: status;
		yourself.
	self testSuiteSample testCases add: testCase.
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printStart [
	self testSuiteSample
		suiteName: runner suiteName;
		timeStamp: DateAndTime now printString;
		properties: Dictionary new;
		notes: '';
		gsVersion: (System gemVersionAt: #'gsVersion');
		testCases: {};
		resultsSummary:
				(Dictionary new
						at: #'summary' put: runner summary;
						at: #'tests' put: runner totalTests;
						at: #'failures' put: runner failedTests;
						at: #'errors' put: runner erroredTests;
						at: #'passed' put: runner passingTests;
						at: #'time' put: runner suiteTime / 1000.0;
						yourself);
		yourself
]

{ #category : 'printing' }
GsTestStatsTestReporter >> report [
	super report.
	^ self testSuiteSample
]

{ #category : 'accessing' }
GsTestStatsTestReporter >> testSuiteSample [
	^testSuiteSample ifNil: [testSuiteSample := GsTestSuiteSample new]
]
