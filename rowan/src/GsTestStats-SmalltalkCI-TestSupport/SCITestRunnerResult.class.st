Class {
	#name : 'SCITestRunnerResult',
	#superclass : 'Object',
	#instVars : [
		'testCase',
		'testError',
		'time',
		'stack',
		'skipped'
	],
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'strings' }
SCITestRunnerResult class >> ballot [
	^ (Character codePoint: 10007) asString
]

{ #category : 'strings' }
SCITestRunnerResult class >> checkMark [
	^ (Character codePoint: 10003) asString
]

{ #category : 'accessing' }
SCITestRunnerResult >> ansiTitle [
	| prefix |
	prefix := self passed
		ifTrue: [ self class checkMark, GsTestStatsCI ansiReset ]
		ifFalse: [ self class ballot ].
	^ self color, ' ', prefix, ' ', self title, GsTestStatsCI ansiReset
]

{ #category : 'accessing' }
SCITestRunnerResult >> ansiTitleContractedTo: anInteger [
	| prefix |
	prefix := self passed
		ifTrue: [ self class checkMark, GsTestStatsCI ansiReset ]
		ifFalse: [ self class ballot ].
	^ self color, ' ', prefix, ' ', (self title contractTo: anInteger), GsTestStatsCI ansiReset
]

{ #category : 'printing' }
SCITestRunnerResult >> asString [
  "note - both asString and printOn: need to be implemented for portability"

  ^ '#', self testCase selector asString
]

{ #category : 'accessing' }
SCITestRunnerResult >> color [
	self errored ifTrue: [ ^ GsTestStatsCI ansiRed ].
	self failed ifTrue: [ ^ GsTestStatsCI ansiYellow ].
	self passedUnexpectedly ifTrue: [ ^ GsTestStatsCI ansiYellow ].
	^ GsTestStatsCI ansiGreen
]

{ #category : 'accessing' }
SCITestRunnerResult >> errored [
	testError
		ifNotNil: [ ^ self testCase shouldPass and: [ (self testError isKindOf: TestFailure) not ] ].
	^ false
]

{ #category : 'accessing' }
SCITestRunnerResult >> failed [
	testError
		ifNil: [ ^ (self testCase shouldPass not) ]
		ifNotNil: [ ^ testCase shouldPass and: [ testError isKindOf: TestFailure ] ]
]

{ #category : 'accessing' }
SCITestRunnerResult >> foldName [
	^ GsTestStatsCI hash: self asString maxLength: 6
]

{ #category : 'accessing' }
SCITestRunnerResult >> passed [
	testCase shouldPass
		ifTrue: [ ^ testError isNil ]
		ifFalse: [ ^ testError isNil not ]
]

{ #category : 'accessing' }
SCITestRunnerResult >> passedUnexpectedly [
	^ testError isNil and: [ (testCase shouldPass) not ]
]

{ #category : 'printing' }
SCITestRunnerResult >> printOn: aStream [
  "note - both asString and printOn: need to be implemented for portability"

  aStream nextPutAll: self asString
]

{ #category : 'accessing' }
SCITestRunnerResult >> skipped [

	^ skipped ifNil: [ false ]
]

{ #category : 'accessing' }
SCITestRunnerResult >> skipped: anObject [

	skipped := anObject
]

{ #category : 'accessing' }
SCITestRunnerResult >> stack [

	^ stack
]

{ #category : 'accessing' }
SCITestRunnerResult >> stack: anObject [

	stack := anObject
]

{ #category : 'accessing' }
SCITestRunnerResult >> testCase [

	^ testCase
]

{ #category : 'accessing' }
SCITestRunnerResult >> testCase: anObject [

	testCase := anObject
]

{ #category : 'accessing' }
SCITestRunnerResult >> testError [

	self passedUnexpectedly ifTrue: [ 
		^ TestFailure new
			messageText: 'Test passed unexpectedly';
			yourself ].

	^ testError
]

{ #category : 'accessing' }
SCITestRunnerResult >> testError: anObject [

	testError := anObject
]

{ #category : 'accessing' }
SCITestRunnerResult >> time [

	^ time
]

{ #category : 'accessing' }
SCITestRunnerResult >> time: anObject [

	time := anObject
]

{ #category : 'accessing' }
SCITestRunnerResult >> title [
	| timeString skippedText |
	timeString := true
		ifTrue: [ '' ]
		ifFalse: [ GsTestStatsCI stringFor: self time maxDecimalPlaces: 0 ].
	skippedText := self skipped
		ifTrue: [ 'skipped, ' ]
		ifFalse: [ '' ].
	^ self asString , ' (' , skippedText , timeString , 'ms)'
]
