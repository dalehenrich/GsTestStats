Class {
	#name : 'SCIObjectWithSpec',
	#superclass : 'Object',
	#instVars : [
		'spec'
	],
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'accessing' }
SCIObjectWithSpec >> spec [

	^ spec
]

{ #category : 'accessing' }
SCIObjectWithSpec >> spec: anObject [

	spec := anObject
]
