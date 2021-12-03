Class {
	#name : 'GsTestCaseSample',
	#superclass : 'Object',
	#instVars : [
		'suite',
		'className',
		'selector',
		'status',
		'time'
	],
	#category : 'GsTestStats-Samples'
}

{ #category : 'accessing' }
GsTestCaseSample class >> erroredStatus [
	^ #'errors'
]

{ #category : 'accessing' }
GsTestCaseSample class >> failedStatus [
	^ #'failures'
]

{ #category : 'instance creation' }
GsTestCaseSample class >> fromDictionary: sampleDict [
	| newTestCaseSample |
	newTestCaseSample := self new.
	sampleDict
		keysAndValuesDo: [ :key :value | newTestCaseSample perform: (key , ':') asSymbol with: value ].
	^ newTestCaseSample
]

{ #category : 'instance creation' }
GsTestCaseSample class >> new [

	^ self basicNew initialize
]

{ #category : 'accessing' }
GsTestCaseSample class >> passedStatus [
	^ #'passed'
]

{ #category : 'accessing' }
GsTestCaseSample >> className [
	^ className
]

{ #category : 'accessing' }
GsTestCaseSample >> className: aString [
	className := aString
]

{ #category : 'initialization' }
GsTestCaseSample >> initialize [
]

{ #category : 'enumerating' }
GsTestCaseSample >> passed: passedBlock failed: failedBlock errored: erroredBlock [
	self status == #'passed'
		ifTrue: [ ^ passedBlock cull: self ].
	self status == #'errors'
		ifTrue: [ ^ erroredBlock cull: self ].
	self status == #'failures'
		ifTrue: [ ^ failedBlock cull: self ]
]

{ #category : 'printing' }
GsTestCaseSample >> printOn: aStream [ 
	aStream nextPutAll: self className, ' debug: ', self selector printString
]

{ #category : 'accessing' }
GsTestCaseSample >> selector [
	^ selector
]

{ #category : 'accessing' }
GsTestCaseSample >> selector: aString [
	selector := aString asSymbol
]

{ #category : 'initialization' }
GsTestCaseSample >> setStatusErrored [
	self status: self class erroredStatus
]

{ #category : 'initialization' }
GsTestCaseSample >> setStatusFailed [
	self status: self class failedStatus
]

{ #category : 'initialization' }
GsTestCaseSample >> setStatusPassed [
	self status: self class passedStatus
]

{ #category : 'accessing' }
GsTestCaseSample >> status [
	^ status
]

{ #category : 'accessing' }
GsTestCaseSample >> status: aString [
	status := aString asSymbol
]

{ #category : 'accessing' }
GsTestCaseSample >> suite [
	^ suite
]

{ #category : 'accessing' }
GsTestCaseSample >> suite: aGsTestCaseSample [
	suite := aGsTestCaseSample
]

{ #category : 'accessing' }
GsTestCaseSample >> testSummaryOn: strm [
	| res |
	strm
		nextPutAll:
				suite name , ' for GemStone ' , (System gemVersionAt: #'gsVersion') printString;
		lf.

	strm
		nextPutAll: res printString;
		lf.
	strm
		nextPutAll: '  errors';
		lf.
	(res errors collect: [ :each | each printString ]) asArray sort
		do: [ :each | 
			strm
				tab;
				nextPutAll: each;
				lf ].
	res failures size > 0
		ifTrue: [ 
			strm
				nextPutAll: '  failures';
				lf.
			(res failures collect: [ :each | each printString ]) asArray sort
				do: [ :each | 
					strm
						tab;
						nextPutAll: each;
						lf ] ]
]

{ #category : 'accessing' }
GsTestCaseSample >> time [
	^ time ifNil: [ time := 0 ]
]

{ #category : 'accessing' }
GsTestCaseSample >> time: object [
	time := object
]
