Class {
	#name : 'GsTestStatsTestReporter',
	#superclass : 'Object',
	#instVars : [
		'runner',
		'stream',
		'testSuiteSample'
	],
	#category : 'GsTestStats-TestSupport'
}

{ #category : 'executing' }
GsTestStatsTestReporter class >> report: aSCITestRunner [
	^ self subclassResponsibility: #report:
]

{ #category : 'helpers' }
GsTestStatsTestReporter >> newLine [
	stream lf
]

{ #category : 'helpers' }
GsTestStatsTestReporter >> print: aString [
	stream nextPutAll: aString asUnicodeString
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
	self printStart.
	self printResults.
	self printEnd.
	^ self testSuiteSample
]

{ #category : 'accessing' }
GsTestStatsTestReporter >> runner [

	^ runner
]

{ #category : 'accessing' }
GsTestStatsTestReporter >> runner: anObject [

	runner := anObject
]

{ #category : 'accessing' }
GsTestStatsTestReporter >> stream [

	^ stream
]

{ #category : 'accessing' }
GsTestStatsTestReporter >> stream: anObject [

	stream := anObject
]

{ #category : 'helpers' }
GsTestStatsTestReporter >> tab [
	stream nextPut: Character tab
]

{ #category : 'accessing' }
GsTestStatsTestReporter >> testSuiteSample [
	^testSuiteSample ifNil: [testSuiteSample := GsTestSuiteSample new]
]
