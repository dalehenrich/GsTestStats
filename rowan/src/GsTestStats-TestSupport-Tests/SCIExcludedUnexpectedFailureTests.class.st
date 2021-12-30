Class {
	#name : 'SCIExcludedUnexpectedFailureTests',
	#superclass : 'SCIExcludedTests',
	#category : 'GsTestStats-TestSupport-Tests'
}

{ #category : 'testing' }
SCIExcludedUnexpectedFailureTests >> testShouldPass [
	"unexpected failure"
	self error: self printString.
]
