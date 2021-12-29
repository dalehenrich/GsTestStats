Class {
	#name : 'GsTestStatsCI',
	#superclass : 'Object',
	#classVars : [
		'DeprecationWarnings',
		'IsColorful'
	],
	#category : 'GsTestStats-TestSupport'
}

{ #category : 'ansi' }
GsTestStatsCI class >> ansiBlue [
	^ self isColorful
		ifTrue: [ self escape, '[34m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
GsTestStatsCI class >> ansiBold [
	^ self isColorful
		ifTrue: [ self escape, '[1m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
GsTestStatsCI class >> ansiClear [
	^ self isColorful
		ifTrue: [ self escape, '[0K' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
GsTestStatsCI class >> ansiGray [
	^ self isColorful
		ifTrue: [ self escape, '[37m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
GsTestStatsCI class >> ansiGreen [
	^ self isColorful
		ifTrue: [ self escape, '[32m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
GsTestStatsCI class >> ansiRed [
	^ self isColorful
		ifTrue: [ self escape, '[31m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
GsTestStatsCI class >> ansiReset [
	^ self isColorful
		ifTrue: [ self escape, '[0m' ]
		ifFalse: [ '' ]
]

{ #category : 'ansi' }
GsTestStatsCI class >> ansiYellow [
	^ self isColorful
		ifTrue: [ self escape, '[33m' ]
		ifFalse: [ '' ]
]

{ #category : 'helpers' }
GsTestStatsCI class >> deprecated: aMessage [
	self deprecationWarnings
		add: self deprecationWarning , ' (' , aMessage , ')'
]

{ #category : 'helpers' }
GsTestStatsCI class >> deprecatedReset [
	DeprecationWarnings := OrderedCollection new
]

{ #category : 'helpers' }
GsTestStatsCI class >> deprecationWarning [
	| senderMethod |
	senderMethod := (GsProcess _frameContentsAt: 3) first.	"First sender is SmalltalkCI>>deprecated"
	^ senderMethod inClass asString , '>>' , senderMethod selector asString
]

{ #category : 'helpers' }
GsTestStatsCI class >> deprecationWarnings [
	^ DeprecationWarnings ifNil: [ DeprecationWarnings := OrderedCollection new ]
]

{ #category : 'helpers' }
GsTestStatsCI class >> deprecationWarnings: aCollection [
	DeprecationWarnings := aCollection
]

{ #category : 'ansi' }
GsTestStatsCI class >> escape [
	^ self isColorful
		ifTrue: [ Character escape asString ]
		ifFalse: [ '' ]
]

{ #category : 'compatibility' }
GsTestStatsCI class >> getEnv: aKey [
	^ System gemEnvironmentVariable: aKey
]

{ #category : 'compatibility' }
GsTestStatsCI class >> hash: aString maxLength: aLength [
	^ aString hash asHexString asLowercase truncateTo: aLength
]

{ #category : 'helpers' }
GsTestStatsCI class >> isColorful [
	"IsColorful := nil"

	^ IsColorful ifNil: [ IsColorful := GsFile stdout isTerminal ]
]

{ #category : 'helpers' }
GsTestStatsCI class >> isColorful: aBool [
	"IsColorful := nil"

	IsColorful := aBool
]

{ #category : 'helpers' }
GsTestStatsCI class >> isDebug [
	^ (self getEnv: 'SCIII_DEBUG') = 'true'
]

{ #category : 'helpers' }
GsTestStatsCI class >> printDebug: aString [
	GsTestStatsCI isDebug
		ifTrue: [ GsTestStatsCI printToStdout: aString ]
]

{ #category : 'helpers' }
GsTestStatsCI class >> printToStdout: msg [
	self stdout
		nextPutAll: msg;
		cr;
		flush
]

{ #category : 'compatibility' }
GsTestStatsCI class >> smalltalkSelection [

	^ System stoneVersionReport at: 'gsVersion'
]

{ #category : 'helpers' }
GsTestStatsCI class >> stdout [
	^ GsFile stdoutServer
]

{ #category : 'compatibility' }
GsTestStatsCI class >> stringFor: aNumber maxDecimalPlaces: placesDesired [
	^ aNumber asFloat
		asStringUsingFormat:
			{0.
			placesDesired.
			false}
]

{ #category : 'compatibility' }
GsTestStatsCI class >> testRunnerClass [
	^ GsTestStatsCITestRunner
]

{ #category : 'compatibility' }
GsTestStatsCI class >> timeToRun: aBlock [
	^ Time millisecondsElapsedTime: aBlock
]
