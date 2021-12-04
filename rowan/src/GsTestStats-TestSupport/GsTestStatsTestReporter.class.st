Class {
	#name : 'GsTestStatsTestReporter',
	#superclass : 'Object',
	#instVars : [
		'runner',
		'stream'
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
	(stream respondsTo: #'nextPutAllUtf8:')
		ifTrue: [ stream nextPutAllUtf8: aString encodeAsUTF8 ]
		ifFalse: [ stream nextPutAll: aString ]
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printDeprecationWarnings [
	self subclassResponsibility: #'printDeprecationWarnings'
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printEnd [
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printException: aResult [
	self subclassResponsibility: #'printException:'
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printNotPassingResults [
	self subclassResponsibility: #'printNotPassingResults'
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printPassingResults [
	self subclassResponsibility: #'printPassingResults'
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printResults [
	(runner passingTests > 0 and: [ runner hidePassingTests not ])
		ifTrue: [ self printPassingResults ].
	(runner isSuccessful not and: [ runner passingTests < runner totalTests])
		ifTrue: [ self printNotPassingResults ].
  self printDeprecationWarnings.
]

{ #category : 'printing' }
GsTestStatsTestReporter >> printStart [
	self subclassResponsibility: #'printStart'
]

{ #category : 'printing' }
GsTestStatsTestReporter >> report [
	self printStart.
	self printResults.
	self printEnd
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
