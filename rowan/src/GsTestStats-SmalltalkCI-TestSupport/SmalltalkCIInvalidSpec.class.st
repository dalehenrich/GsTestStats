Class {
	#name : 'SmalltalkCIInvalidSpec',
	#superclass : 'SmalltalkCISpec',
	#instVars : [
		'originalError'
	],
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'instance creation' }
SmalltalkCIInvalidSpec class >> withError: anError [
	^ self new
		originalError: anError;
		yourself
]

{ #category : 'helpers' }
SmalltalkCIInvalidSpec >> isValid [
	^ false
]

{ #category : 'accessing' }
SmalltalkCIInvalidSpec >> originalError [

	^ originalError
]

{ #category : 'accessing' }
SmalltalkCIInvalidSpec >> originalError: anObject [

	originalError := anObject
]
