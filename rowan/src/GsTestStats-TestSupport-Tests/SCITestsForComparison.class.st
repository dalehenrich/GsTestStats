"
when comparing tests results between different runs, I need 3 sets of passing, failing, and erroring tests so I can test the different combos of formerly passing tests passing, failing or erroring ... and so on ...
"
Class {
	#name : 'SCITestsForComparison',
	#superclass : 'TestCase',
	#category : 'GsTestStats-TestSupport-Tests'
}

{ #category : 'testing' }
SCITestsForComparison >> testError_1 [
	self error: 'ERROR'
]

{ #category : 'testing' }
SCITestsForComparison >> testError_2 [
	self error: 'ERROR'
]

{ #category : 'testing' }
SCITestsForComparison >> testError_3 [
	self error: 'ERROR'
]

{ #category : 'testing' }
SCITestsForComparison >> testFail_1 [
	self assert: false
]

{ #category : 'testing' }
SCITestsForComparison >> testFail_2 [
	self assert: false
]

{ #category : 'testing' }
SCITestsForComparison >> testFail_3 [
	self assert: false
]

{ #category : 'testing' }
SCITestsForComparison >> testPass_1 [
	self assert: true
]

{ #category : 'testing' }
SCITestsForComparison >> testPass_2 [
	self assert: true
]

{ #category : 'testing' }
SCITestsForComparison >> testPass_3 [
	self assert: true
]
