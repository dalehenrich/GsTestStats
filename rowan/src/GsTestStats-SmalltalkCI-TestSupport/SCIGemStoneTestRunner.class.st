Class {
	#name : 'SCIGemStoneTestRunner',
	#superclass : 'SCITestRunner',
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'private' }
SCIGemStoneTestRunner >> serializeError: error of: aTestCase [
  SmalltalkCI platformClass isTravisBuild
    ifFalse: [ 
      "only snap off continuation if doing a local build where developer can access continuations"
      (Rowan globalNamed: 'DebuggerLogEntry')
        createContinuationLabeled: self suiteName , ': ' , aTestCase printString.
      SmalltalkCI platformClass
        commitTransaction: 'attempting to log test case defect continuation' ]
]

{ #category : 'private' }
SCIGemStoneTestRunner >> stackTraceString: err of: aTestCase [
	^ GsProcess stackReportToLevel: 100
]
