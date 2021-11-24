Class {
	#name : 'SCITestRunnerTest',
	#superclass : 'TestCase',
	#category : 'GsTestStats-SmalltalkCI-TestSupport-Tests'
}

{ #category : 'testing' }
SCITestRunnerTest >> testRunClasses [
	| runner |
	runner := GsTestStatsCI testRunnerClass runClasses: {Rowan globalNamed: 'SmalltalkCITest'}.
	self assert: runner isSuccessful.
	runner := GsTestStatsCI testRunnerClass runClasses: {Rowan globalNamed: 'SCIExcludedTests'}.
	self deny: runner isSuccessful.
	self
		should: [ GsTestStatsCI testRunnerClass runClasses: {GsTestStatsCI} ]
		raise: Error
]

{ #category : 'testing' }
SCITestRunnerTest >> testRunSpecNamed [
	| runner |
	runner := GsTestStatsCI testRunnerClass
		runSpec: ((Rowan globalNamed: 'SCIDummySpec') forClass: (Rowan globalNamed: 'SmalltalkCITest')).
	self assert: runner isSuccessful.
	runner := GsTestStatsCI testRunnerClass
		runSpec: ((Rowan globalNamed: 'SCIDummySpec') forClass: (Rowan globalNamed: 'SCIExcludedTests')).
	self deny: runner isSuccessful.
	runner := GsTestStatsCI testRunnerClass
		runSpec: ((Rowan globalNamed: 'SCIDummySpec') forClass: GsTestStatsCI).
	self assert: 0 equals: runner totalTests.
	self assert: runner isSuccessful.
]
