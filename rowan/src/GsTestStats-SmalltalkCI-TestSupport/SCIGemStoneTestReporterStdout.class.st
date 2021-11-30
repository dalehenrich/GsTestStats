Class {
	#name : 'SCIGemStoneTestReporterStdout',
	#superclass : 'SCITestReporterStdout',
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'helpers' }
SCIGemStoneTestReporterStdout >> newLine [
	stream lf
]

{ #category : 'helpers' }
SCIGemStoneTestReporterStdout >> print: aString [
	stream nextPutAll: ((Rowan globalNamed: 'UTF8Encoding') encode: aString)
]
