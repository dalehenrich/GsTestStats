Class {
	#name : 'SmalltalkCI',
	#superclass : 'SCIObjectWithSpec',
	#instVars : [
		'projectDirectory'
	],
	#classVars : [
		'IsColorful'
	],
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'ansi' }
SmalltalkCI class >> ansiBlue [
	^ self isColorful
		ifTrue: [ self escape, '[34m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
SmalltalkCI class >> ansiBold [
	^ self isColorful
		ifTrue: [ self escape, '[1m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
SmalltalkCI class >> ansiClear [
	^ self isColorful
		ifTrue: [ self escape, '[0K' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
SmalltalkCI class >> ansiGray [
	^ self isColorful
		ifTrue: [ self escape, '[37m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
SmalltalkCI class >> ansiGreen [
	^ self isColorful
		ifTrue: [ self escape, '[32m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
SmalltalkCI class >> ansiRed [
	^ self isColorful
		ifTrue: [ self escape, '[31m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
SmalltalkCI class >> ansiReset [
	^ self isColorful
		ifTrue: [ self escape, '[0m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
SmalltalkCI class >> ansiYellow [
	^ self isColorful
		ifTrue: [ self escape, '[33m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
SmalltalkCI class >> escape [
	^ Character escape asString
]

{ #category : 'helpers' }
SmalltalkCI class >> isColorful [
	"IsColorful := nil"
	^ IsColorful ifNil: [ IsColorful := (self getEnv: 'SCIII_COLORFUL') = 'true' ]
]

{ #category : 'compatibility' }
SmalltalkCI class >> getEnv: aKey [
	^ System gemEnvironmentVariable: aKey
]
