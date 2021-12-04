Extension { #name : 'TestCase' }

{ #category : '*gsteststats-kernel-extensions' }
TestCase class >> addToSuite: suite fromMethods: testMethods [
	testMethods do: [ :selector | suite addTest: (self selector: selector) ].
	^ suite
]

{ #category : '*gsteststats-kernel-extensions' }
TestCase class >> addToSuiteFromSelectors: suite [
	^ self
		addToSuite: suite
		fromMethods:
			(self shouldInheritSelectors
				ifTrue: [ self allTestSelectors ]
				ifFalse: [ self testSelectors ])
]

{ #category : '*gsteststats-kernel-extensions' }
TestCase >> debug: aResult [
	aResult debugCase: self
]

{ #category : '*gsteststats-kernel-extensions' }
TestCase >> fail: aDescriptionString [
	^ self assert: false description: aDescriptionString
]

{ #category : '*gsteststats-kernel-extensions' }
TestCase >> shouldPass [
	"expected failures not supported at the moment, so all tests should pass"

	^ true
]
