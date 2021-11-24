Class {
	#name : 'GsTestStatsCI',
	#superclass : 'Object',
	#classVars : [
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

{ #category : 'ansi' }
GsTestStatsCI class >> escape [
	^ Character escape asString
]

{ #category : 'compatibility' }
GsTestStatsCI class >> getEnv: aKey [
	^ System gemEnvironmentVariable: aKey
]

{ #category : 'helpers' }
GsTestStatsCI class >> isColorful [
	"IsColorful := nil"
	^ IsColorful ifNil: [ IsColorful := (self getEnv: 'SCIII_COLORFUL') = 'true' ]
]

{ #category : 'accessing' }
GsTestStatsCI class >> platformClass [
]
