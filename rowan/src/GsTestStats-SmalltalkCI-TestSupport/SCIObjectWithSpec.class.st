Class {
	#name : 'SCIObjectWithSpec',
	#superclass : 'Object',
	#instVars : [
		'spec'
	],
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'instance creation' }
SCIObjectWithSpec class >> new [
	^ self basicNew initialize
]

{ #category : 'Initialization' }
SCIObjectWithSpec >> initialize [
  "Default implementation"  
  ^ self
]

{ #category : 'accessing' }
SCIObjectWithSpec >> spec [

	^ spec
]

{ #category : 'accessing' }
SCIObjectWithSpec >> spec: anObject [

	spec := anObject
]
