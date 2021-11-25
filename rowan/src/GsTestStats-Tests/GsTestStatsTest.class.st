Class {
	#name : 'GsTestStatsTest',
	#superclass : 'TestCase',
	#category : 'GsTestStats-Tests'
}

{ #category : 'tests' }
GsTestStatsTest >> testSmalltalkCISpec_01 [
	"GsTestStats test runs will be run from a Smalltalk script, so SmalltalkCISpec will be built programatically"

	| spec suite runner result |
	spec := SmalltalkCISpec new
		name: 'A test';
		yourself.
	suite := ExampleSetTest suite.
	runner := SCITestRunner runSuite: suite spec: spec.

	result := GsTestStatsTestReporter report: runner.
	self halt
]
