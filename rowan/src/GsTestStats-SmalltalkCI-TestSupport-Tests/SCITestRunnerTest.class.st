Class {
	#name : 'SCITestRunnerTest',
	#superclass : 'TestCase',
	#category : 'GsTestStats-SmalltalkCI-TestSupport-Tests'
}

{ #category : 'testing' }
SCITestRunnerTest >> testRunClasses [
	| runner |
	runner := SmalltalkCI testRunnerClass runClasses: {Rowan globalNamed: 'SmalltalkCITest'}.
	self assert: runner isSuccessful.
	runner := SmalltalkCI testRunnerClass runClasses: {Rowan globalNamed: 'SCIExcludedTests'}.
	self deny: runner isSuccessful.
	self
		should: [ SmalltalkCI testRunnerClass runClasses: {SmalltalkCI} ]
		raise: Error
]

{ #category : 'testing' }
SCITestRunnerTest >> testRunSpecNamed [
	| runner |
	runner := SmalltalkCI testRunnerClass
		runSpec: ((Rowan globalNamed: 'SCIDummySpec') forClass: (Rowan globalNamed: 'SmalltalkCITest')).
	self assert: runner isSuccessful.
	runner := SmalltalkCI testRunnerClass
		runSpec: ((Rowan globalNamed: 'SCIDummySpec') forClass: (Rowan globalNamed: 'SCIExcludedTests')).
	self deny: runner isSuccessful.
	runner := SmalltalkCI testRunnerClass
		runSpec: ((Rowan globalNamed: 'SCIDummySpec') forClass: SmalltalkCI).
	self assert: 0 equals: runner totalTests.
	self assert: runner isSuccessful.
]
