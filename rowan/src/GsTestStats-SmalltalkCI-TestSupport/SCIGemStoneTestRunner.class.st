Class {
	#name : 'SCIGemStoneTestRunner',
	#superclass : 'SCITestRunner',
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'private' }
SCIGemStoneTestRunner >> serializeError: error of: aTestCase [
	self halt: 'implement if needed ... we are serializing errors to json if that matters'
]

{ #category : 'private' }
SCIGemStoneTestRunner >> stackTraceString: err of: aTestCase [
	^ GsProcess stackReportToLevel: 100
]
