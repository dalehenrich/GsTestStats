Class {
	#name : 'GsTestStatsCITestReporterStdout',
	#superclass : 'GsTestStatsTestReporter',
	#category : 'GsTestStats-TestSupport'
}

{ #category : 'executing' }
GsTestStatsCITestReporterStdout class >> report: aSCITestRunner [
	^ self report: aSCITestRunner on: GsTestStatsCI stdout
]

{ #category : 'executing' }
GsTestStatsCITestReporterStdout class >> report: aSCITestRunner on: aStream [
	^ self new
		runner: aSCITestRunner;
		stream: aStream;
		report;
		yourself
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printBanner: aTitle color: aColor [
	self printBanner: aTitle subtitle: '' color: aColor
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printBanner: aTitle subtitle: aSubtitle color: aColor [
	| textLength separator |
	textLength := (aTitle size max: aSubtitle size).
	separator := String new: (textLength + 4) withAll: $#. "+ 4 for `#  #` (see below)"
	self newLine;
		setModeBold; print: aColor, separator; newLine;
		setModeBold; print: aColor, '# ', (self rightPad: aTitle to: textLength), ' #'; newLine.
	aSubtitle ifNotEmpty: [
		self setModeBold; print: aColor, '# ', (self rightPad: aSubtitle to: textLength), ' #'; newLine ].
	self
		setModeBold; print: aColor, separator; newLine;
		resetMode
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printDeprecationWarnings [
	GsTestStatsCI deprecationWarnings ifNotEmpty: [
    self
      printTravisFold: 'deprecation_warnings' action: 'start';
      setModeBoldRed;
      print: 'smalltalkCI Deprecation Warnings';
      resetMode;
      newLine.
    GsTestStatsCI deprecationWarnings do: [ :each | self print: ' - ', each; newLine ].
    self printTravisFold: 'deprecation_warnings' action: 'end' ].
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printEnd [
	self newLine; newLine; setModeBold.
	runner erroredTests > 0
		ifTrue: [ self setModeRed ]
		ifFalse: [
			runner failedTests > 0
				ifTrue: [ self setModeYellow ]
				ifFalse: [ self setModeGreen ] ].

	self
		print: '  Executed ', runner summary, '.';
		resetMode; newLine; newLine
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printException: aResult [
	self
		printTravisFold: aResult foldName action: 'start';
		setModeBold;
		print: (aResult ansiTitleContractedTo: 70);
		newLine;
		setModeBold;
		resetMode.
	aResult stack ifNotNil: [
		self newLine;
			print: aResult stack;
			resetMode ].
	self printTravisFold: aResult foldName action: 'end'
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printNotPassingResults [
	| title |
	title := (runner erroredTests + runner failedTests) asString, ' tests did not pass:'.
	self printBanner: title color: GsTestStatsCI ansiRed.
	runner results keysAndValuesDo: [ :class :results | | notPassing |
		notPassing := results select: [ :result | result passed not ].
		notPassing ifNotEmpty: [
			self printTitle: class asString.
			notPassing do: [ :result | self printException: result ] ] ].
	GsTestStatsCI isTravisBuild
		ifFalse: [ self printNotPassingSummary ]
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printNotPassingSummary [
	self newLine; printBanner: 'Summary' color: GsTestStatsCI ansiRed.
	runner results keysAndValuesDo: [ :class :results | | notPassing |
		notPassing := results select: [ :result | result passed not ].
		notPassing ifNotEmpty: [
			self printTitle: class asString.
			notPassing do: [ :result | self print: result ansiTitle; newLine ] ] ]
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printPass: aResult [
	self print: aResult ansiTitle; newLine
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printPassingResults [
	| title |
	self newLine; printTravisFold: 'passing_tests' action: 'start'.
	title := '(', runner passingTests asString, ' tests passed)'.
	self print: title; newLine.
	
	[ runner results keysAndValuesDo: [ :class :results | | passing |
		passing := results select: [ :result | result passed ].
		passing ifNotEmpty: [
			self printTitle: class asString.
			passing do: [ :result | self printPass: result ] ] ]
	] ensure: [ self printTravisFold: 'passing_tests' action: 'end' ]
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printStart [
	self printBanner: runner suiteName subtitle: runner summary color: GsTestStatsCI ansiBlue
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printTitle: aTitle [
	self
		setModeBold;
		newLine; print: aTitle; newLine;
		resetMode
]

{ #category : 'printing' }
GsTestStatsCITestReporterStdout >> printTravisFold: foldName action: action [
	GsTestStatsCI isTravisBuild ifFalse: [ ^ self ].
	self print: (GsTestStatsCI travisFold: foldName action: action)
]

{ #category : 'ansi' }
GsTestStatsCITestReporterStdout >> resetMode [
	self print: GsTestStatsCI ansiReset
]

{ #category : 'helpers' }
GsTestStatsCITestReporterStdout >> rightPad: aString to: length [
	^ aString, (String new: (length - aString size) withAll: Character space)
]

{ #category : 'ansi' }
GsTestStatsCITestReporterStdout >> setModeBlue [
	self print: GsTestStatsCI ansiBlue
]

{ #category : 'ansi' }
GsTestStatsCITestReporterStdout >> setModeBold [
	self print: GsTestStatsCI ansiBold
]

{ #category : 'ansi' }
GsTestStatsCITestReporterStdout >> setModeBoldBlue [
	self setModeBold; setModeBlue
]

{ #category : 'ansi' }
GsTestStatsCITestReporterStdout >> setModeBoldRed [
	self setModeBold; setModeRed
]

{ #category : 'ansi' }
GsTestStatsCITestReporterStdout >> setModeGreen [
	self print: GsTestStatsCI ansiGreen
]

{ #category : 'ansi' }
GsTestStatsCITestReporterStdout >> setModeRed [
	self print: GsTestStatsCI ansiRed
]

{ #category : 'ansi' }
GsTestStatsCITestReporterStdout >> setModeYellow [
	self print: GsTestStatsCI ansiYellow
]
