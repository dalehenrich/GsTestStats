Class {
	#name : 'GsTestStatsCIDebugUnexpectedFailuresTestRunner',
	#superclass : 'GsTestStatsCITestRunner',
	#instVars : [
		'expectedFailuresTestSuiteSample'
	],
	#category : 'GsTestStats-TestSupport'
}

{ #category : 'running' }
GsTestStatsCIDebugUnexpectedFailuresTestRunner >> debugAll [
	"debug only test cases that are expected to pass"

	| testCaseClassMap |
	testCaseClassMap := self expectedFailuresTestSuiteSample _testCasesMap.
	suite tests
		do: [ :testCase | 
			(testCaseClassMap at: testCase class name ifAbsent: [  ])
				ifNotNil: [ :selectorMap | 
					(selectorMap at: testCase selector ifAbsent: [  ])
						ifNotNil: [ :sampleTestCase | 
							sampleTestCase
								passed: [ 
									"passing test case, debug it"
									testCase debug: self ]
								failed: [ testCase run: self ]
								errored: [ testCase run: self ] ]
						ifNil: [ 
							"new test case debug it"
							testCase debug: self ] ]
				ifNil: [ 
					"new test case class, so debug it"
					testCase debug: self ] ]
]

{ #category : 'accessing' }
GsTestStatsCIDebugUnexpectedFailuresTestRunner >> expectedFailuresTestSuiteSample [
	^expectedFailuresTestSuiteSample
]

{ #category : 'accessing' }
GsTestStatsCIDebugUnexpectedFailuresTestRunner >> expectedFailuresTestSuiteSample: object [
	expectedFailuresTestSuiteSample := object
]
