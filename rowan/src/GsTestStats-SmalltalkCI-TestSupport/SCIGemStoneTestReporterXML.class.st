Class {
	#name : 'SCIGemStoneTestReporterXML',
	#superclass : 'SCITestReporterXML',
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'executing' }
SCIGemStoneTestReporterXML class >> report: aSCITestRunner spec: aSCISpec xmlLogDirPath: xmlLogDirPath [
	| cacheStream |
	cacheStream := WriteStreamPortable on: String new.
	[ 
	self report: aSCITestRunner spec: aSCISpec on: cacheStream.
	(self xmlFileNamed: aSCITestRunner suiteName in: xmlLogDirPath)
		writeStreamDo: [ :stream | stream nextPutAll: cacheStream ] ]
		ensure: [ cacheStream close ]
]

{ #category : 'helpers' }
SCIGemStoneTestReporterXML class >> xmlFileNamed: aName in: aPath [
  | fileName |
  fileName := aName, '.xml'.
  aPath ifNotNil: [
	^ aPath, (Rowan globalNamed: 'SmalltalkCIGemstone') pathNameDelimiter, fileName ].
  ^ (System gemEnvironmentVariable: 'SMALLTALK_CI_BUILD')
    ifNotNil: [ :smalltalk_ci_build | smalltalk_ci_build , (Rowan globalNamed: 'SmalltalkCIGemstone') pathNameDelimiter, fileName ]
    ifNil: [ fileName ]
]
