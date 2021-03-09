Class {
	#name : 'GtsTestCaseSample',
	#superclass : 'Object',
	#instVars : [
		'suite',
		'className',
		'selector',
		'status'
	],
	#category : 'GsTestStats-Samples'
}

{ #category : 'instance creation' }
GtsTestCaseSample class >> fromDictionary: sampleDict [
	| newTestCaseSample |
	newTestCaseSample := self new.
	sampleDict
		keysAndValuesDo: [ :key :value | newTestCaseSample perform: (key , ':') asSymbol with: value ].
	^ newTestCaseSample
]

{ #category : 'instance creation' }
GtsTestCaseSample class >> new [

	^ self basicNew initialize
]

{ #category : 'accessing' }
GtsTestCaseSample >> className [
	^ className
]

{ #category : 'accessing' }
GtsTestCaseSample >> className: aString [
	className := aString
]

{ #category : 'initialization' }
GtsTestCaseSample >> initialize [
]

{ #category : 'enumerating' }
GtsTestCaseSample >> passed: passedBlock failed: failedBlock errored: erroredBlock [
	self status == #'passed'
		ifTrue: [ ^ passedBlock cull: self ].
	self status == #'errors'
		ifTrue: [ ^ erroredBlock cull: self ].
	self status == #'failures'
		ifTrue: [ ^ failedBlock cull: self ]
]

{ #category : 'printing' }
GtsTestCaseSample >> printOn: aStream [ 
	aStream nextPutAll: self className, ' debug: ', self selector printString
]

{ #category : 'accessing' }
GtsTestCaseSample >> selector [
	^ selector
]

{ #category : 'accessing' }
GtsTestCaseSample >> selector: aString [
	selector := aString asSymbol
]

{ #category : 'accessing' }
GtsTestCaseSample >> status [
	^ status
]

{ #category : 'accessing' }
GtsTestCaseSample >> status: aString [
	status := aString asSymbol
]

{ #category : 'accessing' }
GtsTestCaseSample >> suite [
	^ suite
]

{ #category : 'accessing' }
GtsTestCaseSample >> suite: aGtsTestCaseSample [
	suite := aGtsTestCaseSample
]

{ #category : 'accessing' }
GtsTestCaseSample >> testSummaryOn: strm [
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
