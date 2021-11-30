Class {
	#name : 'SCIGemStoneTestRunner',
	#superclass : 'SCITestRunner',
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'private' }
SCIGemStoneTestRunner >> serializeError: error of: aTestCase [
	"implement if needed ... we are serializing errors to json if that matters"

	"for now this is more work than necessary"

	
]

{ #category : 'private' }
SCIGemStoneTestRunner >> stackTraceString: err of: aTestCase [
	true
		ifTrue: [ ^ nil ]
		ifFalse: [ ^ GsProcess stackReportToLevel: 100 ]
]

{ #category : 'accessing' }
SCIGemStoneTestRunner >> suiteTimeString [
	^ 's'
]
