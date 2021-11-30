Class {
	#name : 'GsTestStatsCITestRunnerResult',
	#superclass : 'Object',
	#instVars : [
		'testCase',
		'testError',
		'time',
		'stack',
		'skipped'
	],
	#category : 'GsTestStats-TestSupport'
}

{ #category : 'strings' }
GsTestStatsCITestRunnerResult class >> ballot [
	^ (Character codePoint: 10007) asString
]

{ #category : 'strings' }
GsTestStatsCITestRunnerResult class >> checkMark [
	^ (Character codePoint: 10003) asString
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> ansiTitle [
	| prefix |
	prefix := self passed
		ifTrue: [ self class checkMark, GsTestStatsCI ansiReset ]
		ifFalse: [ self class ballot ].
	^ self color, ' ', prefix, ' ', self title, GsTestStatsCI ansiReset
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> ansiTitleContractedTo: anInteger [
	| prefix |
	prefix := self passed
		ifTrue: [ self class checkMark, GsTestStatsCI ansiReset ]
		ifFalse: [ self class ballot ].
	^ self color, ' ', prefix, ' ', (self title contractTo: anInteger), GsTestStatsCI ansiReset
]

{ #category : 'printing' }
GsTestStatsCITestRunnerResult >> asString [
  "note - both asString and printOn: need to be implemented for portability"

  ^ '#', self testCase selector asString
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> color [
	self errored ifTrue: [ ^ GsTestStatsCI ansiRed ].
	self failed ifTrue: [ ^ GsTestStatsCI ansiYellow ].
	self passedUnexpectedly ifTrue: [ ^ GsTestStatsCI ansiYellow ].
	^ GsTestStatsCI ansiGreen
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> errored [
	testError
		ifNotNil: [ ^ self testCase shouldPass and: [ (self testError isKindOf: TestFailure) not ] ].
	^ false
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> failed [
	testError
		ifNil: [ ^ (self testCase shouldPass not) ]
		ifNotNil: [ ^ testCase shouldPass and: [ testError isKindOf: TestFailure ] ]
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> foldName [
	^ GsTestStatsCI hash: self asString maxLength: 6
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> passed [
	testCase shouldPass
		ifTrue: [ ^ testError isNil ]
		ifFalse: [ ^ testError isNil not ]
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> passedUnexpectedly [
	^ testError isNil and: [ (testCase shouldPass) not ]
]

{ #category : 'printing' }
GsTestStatsCITestRunnerResult >> printOn: aStream [
  "note - both asString and printOn: need to be implemented for portability"

  aStream nextPutAll: self asString
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> skipped [

	^ skipped ifNil: [ false ]
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> skipped: anObject [

	skipped := anObject
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> stack [

	^ stack
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> stack: anObject [

	stack := anObject
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> testCase [

	^ testCase
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> testCase: anObject [

	testCase := anObject
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> testError [

	self passedUnexpectedly ifTrue: [ 
		^ TestFailure new
			messageText: 'Test passed unexpectedly';
			yourself ].

	^ testError
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> testError: anObject [

	testError := anObject
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> time [

	^ time
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> time: anObject [

	time := anObject
]

{ #category : 'accessing' }
GsTestStatsCITestRunnerResult >> title [
	| timeString skippedText |
	timeString := true
		ifTrue: [ '' ]
		ifFalse: [ GsTestStatsCI stringFor: self time maxDecimalPlaces: 0 ].
	skippedText := self skipped
		ifTrue: [ 'skipped, ' ]
		ifFalse: [ '' ].
	^ self asString , ' (' , skippedText , timeString , 'ms)'
]
