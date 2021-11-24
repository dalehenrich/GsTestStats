Class {
	#name : 'GsTestStatsTest',
	#superclass : 'TestCase',
	#category : 'GsTestStats-Tests'
}

{ #category : 'tests' }
GsTestStatsTest >> testSmalltalkCISpec_01 [
	"GsTestStats test runs will be run from a Smalltalk script, so SmalltalkCISpec will be built programatically"

	| spec suite runner stream reporter result|
	spec := SmalltalkCISpec new
		name: 'A test';
		yourself.
	suite := ExampleSetTest suite.
	runner := SCITestRunner runSuite: suite spec: spec.

	stream := WriteStream on: String new.
	reporter := SCITestReporterXML report: runner on: stream.
	result := stream contents.
]
