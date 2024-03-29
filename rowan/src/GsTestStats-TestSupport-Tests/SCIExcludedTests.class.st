Class {
	#name : 'SCIExcludedTests',
	#superclass : 'TestCase',
	#category : 'GsTestStats-TestSupport-Tests'
}

{ #category : 'testing' }
SCIExcludedTests >> testAssertError [
	self assert: 3 equals: 4
]

{ #category : 'testing' }
SCIExcludedTests >> testDeprecation [
	GsTestStatsCI deprecated: 'This is just a test'
]

{ #category : 'testing' }
SCIExcludedTests >> testError [
	self error: 'An error message.'
]

{ #category : 'testing' }
SCIExcludedTests >> testFailure [
	self fail: 'A failure message.'
]

{ #category : 'testing' }
SCIExcludedTests >> testShouldFail [
	self error: 'Foo'.
]

{ #category : 'testing' }
SCIExcludedTests >> testShouldPass [
]

{ #category : 'testing' }
SCIExcludedTests >> testShouldPassUnexpectedly [
]

{ #category : 'testing' }
SCIExcludedTests >> testThisIsAVeryLongMethodNameThatProbablyNeedsToBeContractedInOrderToBeDisplayedCorrectlyInATravisLog [
	self assert: false
]
