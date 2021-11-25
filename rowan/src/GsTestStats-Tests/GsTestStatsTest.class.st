Class {
	#name : 'GsTestStatsTest',
	#superclass : 'TestCase',
	#category : 'GsTestStats-Tests'
}

{ #category : 'tests' }
GsTestStatsTest >> testSmalltalkCISpec_01 [
	"GsTestStats test runs will be run from a Smalltalk script, so SmalltalkCISpec will be built programatically"

	| spec suite runner testStatsResult stream streamResult |
	spec := SmalltalkCISpec new
		name: 'A test';
		yourself.
	suite := ExampleSetTest suite.
	runner := SCITestRunner runSuite: suite spec: spec.

	testStatsResult := GsTestStatsTestReporter report: runner.

	stream := WriteStream on: String new.
	SCITestReporterStdout
		 report: runner spec: spec on: stream.
	streamResult := stream contents.

]
