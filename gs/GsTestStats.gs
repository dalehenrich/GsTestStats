! GsTestStats fileout

! Class Declarations
! Generated file, do not Edit

doit
(Object
	subclass: 'GsTestCaseSample'
	instVarNames: #( suite className selector status time )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'GsTestStats-Samples';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'GsTestStatsCI'
	instVarNames: #(  )
	classVars: #( DeprecationWarnings IsColorful )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'GsTestStats-TestSupport';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'GsTestStatsCITestRunner'
	instVarNames: #( suite suiteTime results properties )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'GsTestStats-TestSupport';
		immediateInvariant.
true.
%

doit
(GsTestStatsCITestRunner
	subclass: 'GsTestStatsCIDebugUnexpectedFailuresTestRunner'
	instVarNames: #( expectedFailuresTestSuiteSample )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'GsTestStats-TestSupport';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'GsTestStatsCITestRunnerResult'
	instVarNames: #( testCase testError time stack skipped )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'GsTestStats-TestSupport';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'GsTestStatsTestReporter'
	instVarNames: #( runner stream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'GsTestStats-TestSupport';
		immediateInvariant.
true.
%

doit
(GsTestStatsTestReporter
	subclass: 'GsTestStatsCITestReporterStdout'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'GsTestStats-TestSupport';
		immediateInvariant.
true.
%

doit
(GsTestStatsTestReporter
	subclass: 'GsTestStatsCITestReporterTestSuiteSample'
	instVarNames: #( testSuiteSample )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'GsTestStats-TestSupport';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'GsTestSuiteSample'
	instVarNames: #( suiteName properties testCases resultsSummary timeStamp notes gsVersion branch commitSha deprecationWarnings )
	classVars: #( SuiteSamples )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'GsTestStats-Samples';
		immediateInvariant.
true.
%

! Class implementation for 'GsTestCaseSample'

!		Class methods for 'GsTestCaseSample'

category: 'accessing'
classmethod: GsTestCaseSample
erroredStatus
	^ #'errors'
%

category: 'accessing'
classmethod: GsTestCaseSample
failedStatus
	^ #'failures'
%

category: 'instance creation'
classmethod: GsTestCaseSample
fromDictionary: sampleDict
	| newTestCaseSample |
	newTestCaseSample := self new.
	sampleDict
		keysAndValuesDo: [ :key :value | newTestCaseSample perform: (key , ':') asSymbol with: value ].
	^ newTestCaseSample
%

category: 'instance creation'
classmethod: GsTestCaseSample
new

	^ self basicNew initialize
%

category: 'accessing'
classmethod: GsTestCaseSample
passedStatus
	^ #'passed'
%

!		Instance methods for 'GsTestCaseSample'

category: 'accessing'
method: GsTestCaseSample
className
	^ className
%

category: 'accessing'
method: GsTestCaseSample
className: aString
	className := aString
%

category: 'initialization'
method: GsTestCaseSample
initialize
%

category: 'enumerating'
method: GsTestCaseSample
passed: passedBlock failed: failedBlock errored: erroredBlock
	self status == #'passed'
		ifTrue: [ ^ passedBlock cull: self ].
	self status == #'errors'
		ifTrue: [ ^ erroredBlock cull: self ].
	self status == #'failures'
		ifTrue: [ ^ failedBlock cull: self ]
%

category: 'printing'
method: GsTestCaseSample
printOn: aStream 
	aStream nextPutAll: self className, ' debug: ', self selector printString
%

category: 'accessing'
method: GsTestCaseSample
selector
	^ selector
%

category: 'accessing'
method: GsTestCaseSample
selector: aString
	selector := aString asSymbol
%

category: 'initialization'
method: GsTestCaseSample
setStatusErrored
	self status: self class erroredStatus
%

category: 'initialization'
method: GsTestCaseSample
setStatusFailed
	self status: self class failedStatus
%

category: 'initialization'
method: GsTestCaseSample
setStatusPassed
	self status: self class passedStatus
%

category: 'accessing'
method: GsTestCaseSample
status
	^ status
%

category: 'accessing'
method: GsTestCaseSample
status: aString
	status := aString asSymbol
%

category: 'accessing'
method: GsTestCaseSample
suite
	^ suite
%

category: 'accessing'
method: GsTestCaseSample
suite: aGsTestCaseSample
	suite := aGsTestCaseSample
%

category: 'accessing'
method: GsTestCaseSample
testSummaryOn: strm
	| res |
	strm
		nextPutAll:
				suite name , ' for GemStone ' , (System gemVersionAt: #'gsVersion') printString;
		lf.

	strm
		nextPutAll: res printString;
		lf.
	strm
		nextPutAll: '  errors';
		lf.
	(res errors collect: [ :each | each printString ]) asArray sort
		do: [ :each | 
			strm
				tab;
				nextPutAll: each;
				lf ].
	res failures size > 0
		ifTrue: [ 
			strm
				nextPutAll: '  failures';
				lf.
			(res failures collect: [ :each | each printString ]) asArray sort
				do: [ :each | 
					strm
						tab;
						nextPutAll: each;
						lf ] ]
%

category: 'accessing'
method: GsTestCaseSample
time
	^ time ifNil: [ time := 0 ]
%

category: 'accessing'
method: GsTestCaseSample
time: object
	time := object
%

! Class implementation for 'GsTestStatsCI'

!		Class methods for 'GsTestStatsCI'

category: 'ansi'
classmethod: GsTestStatsCI
ansiBlue
	^ self isColorful
		ifTrue: [ self escape, '[34m' ]
		ifFalse: [ '' ]
%

category: 'ansi'
classmethod: GsTestStatsCI
ansiBold
	^ self isColorful
		ifTrue: [ self escape, '[1m' ]
		ifFalse: [ '' ]
%

category: 'ansi'
classmethod: GsTestStatsCI
ansiClear
	^ self isColorful
		ifTrue: [ self escape, '[0K' ]
		ifFalse: [ '' ]
%

category: 'ansi'
classmethod: GsTestStatsCI
ansiGray
	^ self isColorful
		ifTrue: [ self escape, '[37m' ]
		ifFalse: [ '' ]
%

category: 'ansi'
classmethod: GsTestStatsCI
ansiGreen
	^ self isColorful
		ifTrue: [ self escape, '[32m' ]
		ifFalse: [ '' ]
%

category: 'ansi'
classmethod: GsTestStatsCI
ansiRed
	^ self isColorful
		ifTrue: [ self escape, '[31m' ]
		ifFalse: [ '' ]
%

category: 'ansi'
classmethod: GsTestStatsCI
ansiReset
	^ self isColorful
		ifTrue: [ self escape, '[0m' ]
		ifFalse: [ '' ]
%

category: 'ansi'
classmethod: GsTestStatsCI
ansiYellow
	^ self isColorful
		ifTrue: [ self escape, '[33m' ]
		ifFalse: [ '' ]
%

category: 'helpers'
classmethod: GsTestStatsCI
deprecated: aMessage
	self deprecationWarnings
		add: self deprecationWarning , ' (' , aMessage , ')'
%

category: 'helpers'
classmethod: GsTestStatsCI
deprecatedReset
	DeprecationWarnings := OrderedCollection new
%

category: 'helpers'
classmethod: GsTestStatsCI
deprecationWarning
	| senderMethod |
	senderMethod := (GsProcess _frameContentsAt: 3) first.	"First sender is SmalltalkCI>>deprecated"
	^ senderMethod inClass asString , '>>' , senderMethod selector asString
%

category: 'helpers'
classmethod: GsTestStatsCI
deprecationWarnings
	^ DeprecationWarnings ifNil: [ DeprecationWarnings := OrderedCollection new ]
%

category: 'helpers'
classmethod: GsTestStatsCI
deprecationWarnings: aCollection
	DeprecationWarnings := aCollection
%

category: 'ansi'
classmethod: GsTestStatsCI
escape
	^ self isColorful
		ifTrue: [ Character escape asString ]
		ifFalse: [ '' ]
%

category: 'compatibility'
classmethod: GsTestStatsCI
getEnv: aKey
	^ System gemEnvironmentVariable: aKey
%

category: 'compatibility'
classmethod: GsTestStatsCI
hash: aString maxLength: aLength
	^ aString hash asHexString asLowercase truncateTo: aLength
%

category: 'helpers'
classmethod: GsTestStatsCI
isColorful
	| tmps tmpsKey |
	tmpsKey := self _tempsIsColorfulKey.
	tmps := SessionTemps current.
	(tmps at: tmpsKey otherwise: nil)
		ifNil: [ 
			| isColorful |
			[ 
			| stdout |
			stdout := GsFile stdout.
			isColorful := stdout isTerminal ]
				on: Error
				do: [ :ignored | false ].
			^ tmps at: tmpsKey put: isColorful ]
		ifNotNil: [ :isColorful | ^ isColorful ]
%

category: 'helpers'
classmethod: GsTestStatsCI
isColorful: aBool
	SessionTemps current at: self _tempsIsColorfulKey put: aBool
%

category: 'helpers'
classmethod: GsTestStatsCI
isDebug
	^ (self getEnv: 'SCIII_DEBUG') = 'true'
%

category: 'helpers'
classmethod: GsTestStatsCI
printDebug: aString
	GsTestStatsCI isDebug
		ifTrue: [ GsTestStatsCI printToStdout: aString ]
%

category: 'helpers'
classmethod: GsTestStatsCI
printToStdout: msg
	self stdout
		nextPutAll: msg;
		cr;
		flush
%

category: 'compatibility'
classmethod: GsTestStatsCI
smalltalkSelection

	^ System stoneVersionReport at: 'gsVersion'
%

category: 'helpers'
classmethod: GsTestStatsCI
stdout
	^ GsFile stdoutServer
%

category: 'compatibility'
classmethod: GsTestStatsCI
stringFor: aNumber maxDecimalPlaces: placesDesired
	^ aNumber asFloat
		asStringUsingFormat:
			{0.
			placesDesired.
			false}
%

category: 'compatibility'
classmethod: GsTestStatsCI
testRunnerClass
	^ GsTestStatsCITestRunner
%

category: 'compatibility'
classmethod: GsTestStatsCI
timeToRun: aBlock
	^ Time millisecondsElapsedTime: aBlock
%

category: 'helpers'
classmethod: GsTestStatsCI
_tempsIsColorfulKey
	^ #'GsTestStatsCI_IsColorful'
%

! Class implementation for 'GsTestStatsCITestRunner'

!		Class methods for 'GsTestStatsCITestRunner'

category: 'running'
classmethod: GsTestStatsCITestRunner
debugClasses: aCollectionOfClasses named: aSuiteName
	| suite classes |
	suite := TestSuite named: aSuiteName.
	classes := (aCollectionOfClasses
		select: [ :each | (each includesBehavior: TestCase) and: [ each isAbstract not ] ])
		asSortedCollection: [ :a :b | a name <= b name ].
	classes do: [ :each | each addToSuiteFromSelectors: suite ].
	^ self debugSuite: suite
%

category: 'running'
classmethod: GsTestStatsCITestRunner
debugSuite: aTestSuite
	^ self new
		initializeOn: aTestSuite;
		debug
%

category: 'running'
classmethod: GsTestStatsCITestRunner
debugUnexpectedFailures: aTestSuite expectedFailures: aGsTestSuiteSample
	^ GsTestStatsCIDebugUnexpectedFailuresTestRunner new
		initializeOn: aTestSuite;
		expectedFailuresTestSuiteSample: aGsTestSuiteSample;
		debug
%

category: 'constants'
classmethod: GsTestStatsCITestRunner
errorExceptions
	^ Halt, Error, TestFailure
%

category: 'compatibility'
classmethod: GsTestStatsCITestRunner
isTestMethod: aMethod
	"Compatibility function for older versions of Squeak that do not have CompiledMethod>>isTestMethod"
	^ (aMethod methodClass isKindOf: TestCase class)
		and: [ aMethod selector beginsWith: 'test' ]
%

category: 'instance creation'
classmethod: GsTestStatsCITestRunner
new
	^ self basicNew initialize
%

category: 'running'
classmethod: GsTestStatsCITestRunner
runClasses: aCollectionOfClasses named: aSuiteName
	| suite classes |
	suite := TestSuite named: aSuiteName.
	classes := (aCollectionOfClasses
		select: [ :each | (each includesBehavior: TestCase) and: [ each isAbstract not ] ])
		asSortedCollection: [ :a :b | a name <= b name ].
	classes do: [ :each | each addToSuiteFromSelectors: suite ].
	^ self runSuite: suite
%

category: 'running'
classmethod: GsTestStatsCITestRunner
runSuite: aTestSuite
	^ self new
		initializeOn: aTestSuite;
		run
%

category: 'constants'
classmethod: GsTestStatsCITestRunner
skipExceptions
	^ ExceptionSet new
%

!		Instance methods for 'GsTestStatsCITestRunner'

category: 'running'
method: GsTestStatsCITestRunner
basicRunCase: aTestCase
	aTestCase runCase	
%

category: 'running'
method: GsTestStatsCITestRunner
debug
	[ 
	self setUp.
	suiteTime := GsTestStatsCI timeToRun: [ self debugAll ] ]
		ensure: [ self tearDown ]
%

category: 'running'
method: GsTestStatsCITestRunner
debugAll
	suite tests do: [ :each | each debug: self ]
%

category: 'running'
method: GsTestStatsCITestRunner
debugCase: aTestCase
	| result |
	GsTestStatsCI printDebug: aTestCase asString.
	result := GsTestStatsCITestRunnerResult new
		testCase: aTestCase;
		yourself.
	self basicRunCase: aTestCase.
	self printProgress: result.
	(results at: aTestCase class ifAbsentPut: [ OrderedCollection new ])
		add: result
%

category: 'accessing'
method: GsTestStatsCITestRunner
erroredTests
	| size |
	size := 0.
	results valuesDo: [ :clsResults |
		size := size + (clsResults select: [ :r | r errored ]) size ].
	^ size
%

category: 'accessing'
method: GsTestStatsCITestRunner
failedTests
	| size |
	size := 0.
	results valuesDo: [ :clsResults |
		size := size + (clsResults select: [ :r | r failed ]) size ].
	^ size
%

category: 'properties'
method: GsTestStatsCITestRunner
failOnSCIDeprecationWarnings
	^ properties at: #'failOnSCIDeprecationWarnings' ifAbsent: [ false ]
%

category: 'properties'
method: GsTestStatsCITestRunner
failOnSCIDeprecationWarnings: object
	properties at: #'failOnSCIDeprecationWarnings' put: object
%

category: 'properties'
method: GsTestStatsCITestRunner
failOnZeroTests
	^ properties at: #'failOnZeroTests' ifAbsent: [ false ]
%

category: 'properties'
method: GsTestStatsCITestRunner
failOnZeroTests: object
	properties at: #'failOnZeroTests' put: object
%

category: 'properties'
method: GsTestStatsCITestRunner
hidePassingTests
	^ properties at: #'hidePassingTests' ifAbsent: [ false ]
%

category: 'properties'
method: GsTestStatsCITestRunner
hidePassingTests: object
	^ properties at: #'hidePassingTests' put: object
%

category: 'initialize-release'
method: GsTestStatsCITestRunner
initialize
	super initialize.
	suiteTime := Duration seconds: 0.
	properties := Dictionary new.
	results := Dictionary new
%

category: 'initialize-release'
method: GsTestStatsCITestRunner
initializeOn: aTestSuite
	suite := aTestSuite
%

category: 'accessing'
method: GsTestStatsCITestRunner
isSuccessful
	| deprecationWarningCheck |
	deprecationWarningCheck := self failOnSCIDeprecationWarnings
		ifTrue: [ GsTestStatsCI deprecationWarnings isEmpty ]
		ifFalse: [ true ].
	^ deprecationWarningCheck and: [
			(self erroredTests + self failedTests) = 0]
%

category: 'accessing'
method: GsTestStatsCITestRunner
passingTests
	^ self totalTests - self erroredTests - self failedTests
%

category: 'running'
method: GsTestStatsCITestRunner
printProgress: aTestResult
	GsTestStatsCI stdout nextPut: (aTestResult passed
		ifTrue: [ $. ]
		ifFalse: [ (aTestResult failed)
			ifTrue: [ $F ]
			ifFalse: [ $E ] ])
%

category: 'accessing'
method: GsTestStatsCITestRunner
properties
	^properties
%

category: 'accessing'
method: GsTestStatsCITestRunner
properties: object
	properties := object
%

category: 'accessing'
method: GsTestStatsCITestRunner
results

	^ results
%

category: 'running'
method: GsTestStatsCITestRunner
run
	[ 
	self setUp.
	suiteTime := GsTestStatsCI timeToRun: [ self runAll ] ]
		ensure: [ self tearDown ]
%

category: 'running'
method: GsTestStatsCITestRunner
runAll
	suite tests do: [ :each | each run: self ]
%

category: 'running'
method: GsTestStatsCITestRunner
runCase: aTestCase
	| result |
	GsTestStatsCI printDebug: aTestCase asString.
	result := GsTestStatsCITestRunnerResult new
		testCase: aTestCase;
		yourself.
	result
		time:
			(GsTestStatsCI
				timeToRun: [ 
					[ 
					[ self basicRunCase: aTestCase ]
						on: self class errorExceptions
						do: [ :err | 
							result testError: err.
							aTestCase shouldPass & self serializeError
								ifTrue: [ result stack: (self stackTraceString: err of: aTestCase) ] ] ]
						on: self class skipExceptions
						do: [ :skip | result skipped: true ] ]).
	self printProgress: result.
	(results at: aTestCase class ifAbsentPut: [ OrderedCollection new ])
		add: result
%

category: 'properties'
method: GsTestStatsCITestRunner
serializeError
	^ properties at: #'serializeError' ifAbsent: [ false ]
%

category: 'properties'
method: GsTestStatsCITestRunner
serializeError: object
	properties at: #'serializeError' put: object
%

category: 'private'
method: GsTestStatsCITestRunner
serializeError: error of: aTestCase
	"Hook for supporting post-mortem stack dumps. Do nothing by default."
%

category: 'running'
method: GsTestStatsCITestRunner
setUp
	GsTestStatsCI printToStdout: 'Running suite "', self suiteName ,'" with ', suite tests size asString, ' tests.'.

	"Initialize the test resources."
	suite resources do: [ :each |
		each isAvailable
			ifFalse: [ TestResult signalErrorWith: 'Resource ' , each name , ' could not be initialized' ] ]
%

category: 'private'
method: GsTestStatsCITestRunner
stackTraceString: err of: aTestCase
	^ GsProcess stackReportToLevel: 100
%

category: 'accessing'
method: GsTestStatsCITestRunner
suite

	^ suite
%

category: 'accessing'
method: GsTestStatsCITestRunner
suite: anObject

	suite := anObject
%

category: 'accessing'
method: GsTestStatsCITestRunner
suiteName

	^ suite name
%

category: 'accessing'
method: GsTestStatsCITestRunner
suiteTime

	^ suiteTime
%

category: 'accessing'
method: GsTestStatsCITestRunner
suiteTimeInSeconds
	^ (self suiteTime / 1000.0)
%

category: 'accessing'
method: GsTestStatsCITestRunner
suiteTimeString
	^ (GsTestStatsCI stringFor: self suiteTimeInSeconds maxDecimalPlaces: 2), 's'
%

category: 'accessing'
method: GsTestStatsCITestRunner
summary
	^ self totalTests asString , ' Tests with ' , self failedTests asString
		, ' Failures, ' , self erroredTests asString , ' Errors and '
		, GsTestStatsCI deprecationWarnings size asString , ' DeprecationWarnings in '
		, self suiteTimeString
%

category: 'running'
method: GsTestStatsCITestRunner
tearDown
	suite resources
		do: [ :each | each reset ].

	GsTestStatsCI printToStdout: 'Finished running suite: ', self suiteName.
	
	(self failOnZeroTests and: [ self totalTests == 0 ])
		ifTrue: [ Error signal: 'No tests were executed.

If this is intended, use `#failOnZeroTests : false` in your SmalltalkCISpec.' ].
%

category: 'accessing'
method: GsTestStatsCITestRunner
totalTests
	| size |
	size := 0.
	results valuesDo: [ :clsResults |
		size := size + clsResults size ].
	^ size
%

! Class implementation for 'GsTestStatsCIDebugUnexpectedFailuresTestRunner'

!		Instance methods for 'GsTestStatsCIDebugUnexpectedFailuresTestRunner'

category: 'running'
method: GsTestStatsCIDebugUnexpectedFailuresTestRunner
debugAll
	"debug only test cases that are expected to pass"

	| testCaseClassMap |
	testCaseClassMap := self expectedFailuresTestSuiteSample _testCasesMap.
	suite tests
		do: [ :testCase | 
			(testCaseClassMap at: testCase class name ifAbsent: [  ])
				ifNotNil: [ :selectorMap | 
					(selectorMap at: testCase selector ifAbsent: [  ])
						ifNotNil: [ :sampleTestCase | 
							sampleTestCase
								passed: [ 
									"passing test case, debug it"
									testCase debug: self ]
								failed: [ testCase run: self ]
								errored: [ testCase run: self ] ]
						ifNil: [ 
							"new test case debug it"
							testCase debug: self ] ]
				ifNil: [ 
					"new test case class, so debug it"
					testCase debug: self ] ]
%

category: 'accessing'
method: GsTestStatsCIDebugUnexpectedFailuresTestRunner
expectedFailuresTestSuiteSample
	^expectedFailuresTestSuiteSample
%

category: 'accessing'
method: GsTestStatsCIDebugUnexpectedFailuresTestRunner
expectedFailuresTestSuiteSample: object
	expectedFailuresTestSuiteSample := object
%

! Class implementation for 'GsTestStatsCITestRunnerResult'

!		Class methods for 'GsTestStatsCITestRunnerResult'

category: 'strings'
classmethod: GsTestStatsCITestRunnerResult
ballot
	^ GsTestStatsCI isColorful
		ifTrue: [ (Character codePoint: 10007) asString ]
		ifFalse: [ '' ]
%

category: 'strings'
classmethod: GsTestStatsCITestRunnerResult
checkMark
	^ GsTestStatsCI isColorful
		ifTrue: [ (Character codePoint: 10003) asString ]
		ifFalse: [ '' ]
%

!		Instance methods for 'GsTestStatsCITestRunnerResult'

category: 'accessing'
method: GsTestStatsCITestRunnerResult
ansiTitle
	| prefix |
	prefix := self passed
		ifTrue: [ self class checkMark, GsTestStatsCI ansiReset ]
		ifFalse: [ self class ballot ].
	^ self color, ' ', prefix, ' ', self title, GsTestStatsCI ansiReset
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
ansiTitleContractedTo: anInteger
	| prefix |
	prefix := self passed
		ifTrue: [ self class checkMark, GsTestStatsCI ansiReset ]
		ifFalse: [ self class ballot ].
	^ self color, ' ', prefix, ' ', (self title contractTo: anInteger), GsTestStatsCI ansiReset
%

category: 'printing'
method: GsTestStatsCITestRunnerResult
asString
  "note - both asString and printOn: need to be implemented for portability"

  ^ '#', self testCase selector asString
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
color
	self errored ifTrue: [ ^ GsTestStatsCI ansiRed ].
	self failed ifTrue: [ ^ GsTestStatsCI ansiYellow ].
	self passedUnexpectedly ifTrue: [ ^ GsTestStatsCI ansiYellow ].
	^ GsTestStatsCI ansiGreen
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
errored
	testError
		ifNotNil: [ ^ self testCase shouldPass and: [ (self testError isKindOf: TestFailure) not ] ].
	^ false
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
failed
	testError
		ifNil: [ ^ (self testCase shouldPass not) ]
		ifNotNil: [ ^ testCase shouldPass and: [ testError isKindOf: TestFailure ] ]
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
foldName
	^ GsTestStatsCI hash: self asString maxLength: 6
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
passed
	testCase shouldPass
		ifTrue: [ ^ testError isNil ]
		ifFalse: [ ^ testError isNil not ]
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
passedUnexpectedly
	^ testError isNil and: [ (testCase shouldPass) not ]
%

category: 'printing'
method: GsTestStatsCITestRunnerResult
printOn: aStream
  "note - both asString and printOn: need to be implemented for portability"

  aStream nextPutAll: self asString
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
skipped

	^ skipped ifNil: [ false ]
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
skipped: anObject

	skipped := anObject
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
stack

	^ stack
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
stack: anObject

	stack := anObject
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
testCase

	^ testCase
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
testCase: anObject

	testCase := anObject
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
testError

	self passedUnexpectedly ifTrue: [ 
		^ TestFailure new
			messageText: 'Test passed unexpectedly';
			yourself ].

	^ testError
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
testError: anObject

	testError := anObject
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
time

	^ time
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
time: anObject

	time := anObject
%

category: 'accessing'
method: GsTestStatsCITestRunnerResult
title
	| timeString skippedText |
	timeString := true
		ifTrue: [ '' ]
		ifFalse: [ GsTestStatsCI stringFor: self time maxDecimalPlaces: 0 ].
	skippedText := self skipped
		ifTrue: [ 'skipped, ' ]
		ifFalse: [ '' ].
	^ self asString , ' (' , skippedText , timeString , 'ms)'
%

! Class implementation for 'GsTestStatsTestReporter'

!		Class methods for 'GsTestStatsTestReporter'

category: 'executing'
classmethod: GsTestStatsTestReporter
report: aSCITestRunner
	^ self subclassResponsibility: #report:
%

!		Instance methods for 'GsTestStatsTestReporter'

category: 'helpers'
method: GsTestStatsTestReporter
newLine
	stream lf
%

category: 'helpers'
method: GsTestStatsTestReporter
print: aString
	(stream respondsTo: #'nextPutAllUtf8:')
		ifTrue: [ stream nextPutAllUtf8: aString encodeAsUTF8 ]
		ifFalse: [ stream nextPutAll: aString ]
%

category: 'printing'
method: GsTestStatsTestReporter
printDeprecationWarnings
	self subclassResponsibility: #'printDeprecationWarnings'
%

category: 'printing'
method: GsTestStatsTestReporter
printEnd
%

category: 'printing'
method: GsTestStatsTestReporter
printException: aResult
	self subclassResponsibility: #'printException:'
%

category: 'printing'
method: GsTestStatsTestReporter
printNotPassingResults
	self subclassResponsibility: #'printNotPassingResults'
%

category: 'printing'
method: GsTestStatsTestReporter
printPassingResults
	self subclassResponsibility: #'printPassingResults'
%

category: 'printing'
method: GsTestStatsTestReporter
printResults
	(runner passingTests > 0 and: [ runner hidePassingTests not ])
		ifTrue: [ self printPassingResults ].
	(runner isSuccessful not and: [ runner passingTests < runner totalTests])
		ifTrue: [ self printNotPassingResults ].
  self printDeprecationWarnings.
%

category: 'printing'
method: GsTestStatsTestReporter
printStart
	self subclassResponsibility: #'printStart'
%

category: 'printing'
method: GsTestStatsTestReporter
report
	self printStart.
	self printResults.
	self printEnd
%

category: 'accessing'
method: GsTestStatsTestReporter
runner

	^ runner
%

category: 'accessing'
method: GsTestStatsTestReporter
runner: anObject

	runner := anObject
%

category: 'accessing'
method: GsTestStatsTestReporter
stream

	^ stream
%

category: 'accessing'
method: GsTestStatsTestReporter
stream: anObject

	stream := anObject
%

category: 'helpers'
method: GsTestStatsTestReporter
tab
	stream nextPut: Character tab
%

! Class implementation for 'GsTestStatsCITestReporterStdout'

!		Class methods for 'GsTestStatsCITestReporterStdout'

category: 'executing'
classmethod: GsTestStatsCITestReporterStdout
report: aSCITestRunner
	^ self report: aSCITestRunner on: GsTestStatsCI stdout
%

category: 'executing'
classmethod: GsTestStatsCITestReporterStdout
report: aSCITestRunner on: aStream
	^ self new
		runner: aSCITestRunner;
		stream: aStream;
		report;
		yourself
%

category: 'executing'
classmethod: GsTestStatsCITestReporterStdout
reportNotPassing: aSCITestRunner
	"do not print the details for passing tests"

	^ self reportNotPassing: aSCITestRunner on: GsTestStatsCI stdout
%

category: 'executing'
classmethod: GsTestStatsCITestReporterStdout
reportNotPassing: aSCITestRunner on: aStream
	"do not print the details for passing tests"

	^ self new
		runner: aSCITestRunner;
		stream: aStream;
		reportNotPassing;
		yourself
%

!		Instance methods for 'GsTestStatsCITestReporterStdout'

category: 'printing'
method: GsTestStatsCITestReporterStdout
printBanner: aTitle color: aColor
	self printBanner: aTitle subtitle: '' color: aColor
%

category: 'printing'
method: GsTestStatsCITestReporterStdout
printBanner: aTitle subtitle: aSubtitle color: aColor
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
%

category: 'printing'
method: GsTestStatsCITestReporterStdout
printDeprecationWarnings
	| warnings |
	warnings := GsTestStatsCI deprecationWarnings.
	warnings
		ifNotEmpty: [ 
			self
				printBanner: warnings size asString , ' smalltalkCI Deprecation Warnings'
				color: GsTestStatsCI ansiRed.
			warnings
				do: [ :each | 
					self
						print: ' - ' , each;
						newLine ] ]
%

category: 'printing'
method: GsTestStatsCITestReporterStdout
printEnd
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
%

category: 'printing'
method: GsTestStatsCITestReporterStdout
printException: aResult
	self
		setModeBold;
		print: aResult ansiTitle;
		newLine;
		setModeBold;
		resetMode.
	aResult stack
		ifNotNil: [ 
			self
				newLine;
				print: aResult stack;
				resetMode ]
%

category: 'printing'
method: GsTestStatsCITestReporterStdout
printNotPassingResults
	| title |
	title := (runner erroredTests + runner failedTests) asString
		, ' tests did not pass:'.
	self printBanner: title color: GsTestStatsCI ansiRed.
	runner results
		keysAndValuesDo: [ :class :results | 
			| notPassing |
			notPassing := results select: [ :result | result passed not ].
			notPassing
				ifNotEmpty: [ 
					self printTitle: class asString.
					notPassing do: [ :result | self printException: result ] ] ].
%

category: 'printing'
method: GsTestStatsCITestReporterStdout
printPass: aResult
	self print: aResult ansiTitle; newLine
%

category: 'printing'
method: GsTestStatsCITestReporterStdout
printPassingResults
	| title |
	title := '(' , runner passingTests asString , ' tests passed)'.
	self
		print: title;
		newLine.

	runner results
		keysAndValuesDo: [ :class :results | 
			| passing |
			passing := results select: [ :result | result passed ].
			passing
				ifNotEmpty: [ 
					self printTitle: class asString.
					passing do: [ :result | self printPass: result ] ] ]
%

category: 'printing'
method: GsTestStatsCITestReporterStdout
printStart
	self printBanner: runner suiteName subtitle: runner summary color: GsTestStatsCI ansiBlue
%

category: 'printing'
method: GsTestStatsCITestReporterStdout
printTitle: aTitle
	self
		setModeBold;
		newLine; print: aTitle; newLine;
		resetMode
%

category: 'printing'
method: GsTestStatsCITestReporterStdout
reportNotPassing
	"do not print the details for passing tests"

	| color |
	self printStart.
	color := runner passingTests > 0
		ifTrue: [ GsTestStatsCI ansiGreen ]
		ifFalse: [ GsTestStatsCI ansiRed ].
	self printBanner: runner passingTests asString , ' tests passed' color: color.
	(runner isSuccessful not and: [ runner passingTests < runner totalTests ])
		ifTrue: [ self printNotPassingResults ].
	self printDeprecationWarnings.
	self printEnd
%

category: 'ansi'
method: GsTestStatsCITestReporterStdout
resetMode
	self print: GsTestStatsCI ansiReset
%

category: 'helpers'
method: GsTestStatsCITestReporterStdout
rightPad: aString to: length
	^ aString, (String new: (length - aString size) withAll: Character space)
%

category: 'ansi'
method: GsTestStatsCITestReporterStdout
setModeBlue
	self print: GsTestStatsCI ansiBlue
%

category: 'ansi'
method: GsTestStatsCITestReporterStdout
setModeBold
	self print: GsTestStatsCI ansiBold
%

category: 'ansi'
method: GsTestStatsCITestReporterStdout
setModeBoldBlue
	self setModeBold; setModeBlue
%

category: 'ansi'
method: GsTestStatsCITestReporterStdout
setModeBoldRed
	self setModeBold; setModeRed
%

category: 'ansi'
method: GsTestStatsCITestReporterStdout
setModeGreen
	self print: GsTestStatsCI ansiGreen
%

category: 'ansi'
method: GsTestStatsCITestReporterStdout
setModeRed
	self print: GsTestStatsCI ansiRed
%

category: 'ansi'
method: GsTestStatsCITestReporterStdout
setModeYellow
	self print: GsTestStatsCI ansiYellow
%

! Class implementation for 'GsTestStatsCITestReporterTestSuiteSample'

!		Class methods for 'GsTestStatsCITestReporterTestSuiteSample'

category: 'executing'
classmethod: GsTestStatsCITestReporterTestSuiteSample
report: aSCITestRunner
	^ self new
		runner: aSCITestRunner;
		report;
		yourself
%

category: 'executing'
classmethod: GsTestStatsCITestReporterTestSuiteSample
report: aSCITestRunner branch: branch
	^ self new
		runner: aSCITestRunner;
		branch: branch commitSha: '';
		report;
		yourself
%

category: 'executing'
classmethod: GsTestStatsCITestReporterTestSuiteSample
report: aSCITestRunner branch: branch commitSha: commitSha
	^ self new
		runner: aSCITestRunner;
		branch: branch commitSha: commitSha;
		report;
		yourself
%

!		Instance methods for 'GsTestStatsCITestReporterTestSuiteSample'

category: 'accessing'
method: GsTestStatsCITestReporterTestSuiteSample
branch: branch commitSha: commitSha
	self testSuiteSample
		branch: branch;
		commitSha: commitSha;
		yourself
%

category: 'printing'
method: GsTestStatsCITestReporterTestSuiteSample
printDeprecationWarnings
	self testSuiteSample deprecationWarnings: GsTestStatsCI deprecationWarnings
%

category: 'printing'
method: GsTestStatsCITestReporterTestSuiteSample
printEnd
%

category: 'printing'
method: GsTestStatsCITestReporterTestSuiteSample
printNotPassingResults
	runner results
		keysAndValuesDo: [ :class :results | 
			| className |
			className := class name asString.
			(results select: [ :result | result errored ])
				do: [ :erorredTestRunnerResult | 
					self testSuiteSample
						addTestCaseSample:
							(GsTestCaseSample new
								className: className;
								selector: erorredTestRunnerResult testCase selector;
								setStatusErrored;
								yourself) ].
			(results select: [ :result | result failed ])
				do: [ :failedTestRunnerResult | 
					self testSuiteSample
						addTestCaseSample:
							(GsTestCaseSample new
								className: className;
								selector: failedTestRunnerResult testCase selector;
								setStatusFailed;
								yourself) ] ]
%

category: 'printing'
method: GsTestStatsCITestReporterTestSuiteSample
printPassingResults
	runner results
		keysAndValuesDo: [ :class :results | 
			| className |
			className := class name asString.
			(results select: [ :result | result passed ])
				do: [ :passingTestRunnerResult | 
					self testSuiteSample
						addTestCaseSample:
							(GsTestCaseSample new
								className: className;
								selector: passingTestRunnerResult testCase selector;
								setStatusPassed;
								yourself) ] ]
%

category: 'printing'
method: GsTestStatsCITestReporterTestSuiteSample
printResult: aResult
	| testCase status |
	status := aResult passed
		ifTrue: [ 'passed' ]
		ifFalse: [ 
			aResult errored
				ifTrue: [ 'errors' ]
				ifFalse: [ 'failures' ] ].
	testCase := GsTestCaseSample new
		className: aResult testCase class name asString;
		selector: aResult testCase selector;
		status: status;
		suite: self testSuiteSample;
		time: aResult time;	"milliseconds"
		yourself.
	self testSuiteSample testCases add: testCase
%

category: 'printing'
method: GsTestStatsCITestReporterTestSuiteSample
printStart
	| dictClass |
	dictClass := (System myUserProfile objectNamed: #'GsTonelOrderedDictionary') ifNil: [ Dictionary ].
	self testSuiteSample
		suiteName: runner suiteName;
		timeStamp: DateAndTime now printString;
		properties: Dictionary new;
		notes: '';
		gsVersion: (System gemVersionAt: #'gsVersion');
		testCases: {};
		resultsSummary:
				(dictClass new
						at: #'errors' put: runner erroredTests;
						at: #'failures' put: runner failedTests;
						at: #'passed' put: runner passingTests;
						at: #'summary' put: runner summary;
						at: #'tests' put: runner totalTests;
						at: #'time' put: runner suiteTime / 1000.0;
						yourself).
%

category: 'accessing'
method: GsTestStatsCITestReporterTestSuiteSample
testSuiteSample
	^testSuiteSample ifNil: [testSuiteSample := GsTestSuiteSample new]
%

! Class implementation for 'GsTestSuiteSample'

!		Class methods for 'GsTestSuiteSample'

category: 'comparison'
classmethod: GsTestSuiteSample
compareLatestTestCasesFor: aGsVersion branch: aBranch to: testSuiteSample on: aWriteStream
	| latestTestSuiteSample |
	latestTestSuiteSample := self
		latestQuerySuiteSampleFor: aBranch
		andVersion: aGsVersion.
	latestTestSuiteSample
		ifNil: [ 
			self
				error:
					'No suite sample found for GemStone ' , aGsVersion printString , '; branch '
						, aBranch printString ].
	^ testSuiteSample compareTo: latestTestSuiteSample on: aWriteStream
%

category: 'instance creation'
classmethod: GsTestSuiteSample
fromDictionary: sampleDict

	| newSuiteSample |
	newSuiteSample := self new.
	sampleDict keysAndValuesDo: [:key :value |
		newSuiteSample perform: (key, ':') asSymbol with: value ].
	^ newSuiteSample
%

category: 'instance creation'
classmethod: GsTestSuiteSample
fromJson: filePath
	| fileStream sampleDict |
	fileStream := GsFile openReadOnServer: filePath.
	sampleDict := STON fromStream: fileStream.
	fileStream close.
	^ self fromDictionary: sampleDict
%

category: 'instance creation'
classmethod: GsTestSuiteSample
fromJsonStream: aStream
	| sample resultsSummary |
	sample := self fromDictionary: (STON fromStream: aStream).
	resultsSummary := sample resultsSummary.
	resultsSummary copy
		keysAndValuesDo: [ :key :value | 
			key _isSymbol
				ifTrue: [ 
					"repair results summary for now"
					resultsSummary at: key asString put: value.
					resultsSummary removeKey: key asSymbol ifAbsent: [  ] ] ].
	^ sample
%

category: 'example'
classmethod: GsTestSuiteSample
generateResultsDictionarFromTestSuite: suite testResult: res
	"results dictionary is suitable for writing out a JSON file"

	| resultsDict resultCases |
	resultsDict := Dictionary new.
	resultCases := {}.
	resultsDict
		at: #'suiteName' put: suite name;
		at: #'timeStamp' put: DateAndTime now printString;
		at: #'properties' put: Dictionary new;
		at: #'notes' put: '';
		at: #'gsVersion' put: (System gemVersionAt: #'gsVersion');
		at: #'testCases' put: resultCases;
		at: #'resultsSummary'
			put:
				(Dictionary new
						at: #'summary' put: res printString;
						at: #'failures' put: res failureCount;
						at: #'errors' put: res errorCount;
						at: #'passed' put: res passedCount;
						yourself) yourself.
	res passed
		do: [ :each | 
			resultCases
				add:
					(Dictionary new
						at: #'className' put: each class asString;
						at: #'selector' put: each selector asString;
						at: #'status' put: 'passed';
						yourself) ].

	res errors
		do: [ :each | 
			resultCases
				add:
					(Dictionary new
						at: #'className' put: each class asString;
						at: #'selector' put: each selector asString;
						at: #'status' put: 'errors';
						yourself) ].
	res failures
		do: [ :each | 
			resultCases
				add:
					(Dictionary new
						at: #'className' put: each class asString;
						at: #'selector' put: each selector asString;
						at: #'status' put: 'failures';
						yourself).
			each printString ].
	^ resultsDict
%

category: 'example'
classmethod: GsTestSuiteSample
generateSampleFromTestSuite: suite testResult: res
	^ self
		fromDictionary:
			(self generateResultsDictionarFromTestSuite: suite testResult: res)
%

category: 'queries'
classmethod: GsTestSuiteSample
latestQuerySuiteSampleFor: branch andVersion: gsVersion
	"return the latest suite sample for the given <branch> and <gsVersion>. 
	With only a GemStone version,
		write the latest sample for that version. With only a branch, write the
		latest sample for that branch.  With no options, write the absolute latest
		sample.
	Return nil if no sample satisfies the query"

	| query stream |
	query := GsTestSuiteSample querySuiteSamples: 'each.timeStamp < x'.
	query
		bind: 'x'
		to:
			DateAndTime now
				+
					(Duration
						days: 1
						hours: 0
						minutes: 0
						seconds: 0).
	stream := query reversedReadStream.
	[ stream atEnd ]
		whileFalse: [ 
			| sample |
			sample := stream next.
			(branch isNil and: [ gsVersion isNil ])
				ifTrue: [ ^ sample ]
				ifFalse: [ 
					branch
						ifNil: [ 
							sample gsVersion = gsVersion
								ifTrue: [ ^ sample ] ]
						ifNotNil: [ 
							gsVersion
								ifNil: [ 
									sample branch = branch
										ifTrue: [ ^ sample ] ]
								ifNotNil: [ 
									(sample branch = branch and: [ sample gsVersion = gsVersion ])
										ifTrue: [ ^ sample ] ] ] ] ].
	^ nil
%

category: 'instance creation'
classmethod: GsTestSuiteSample
new

	^ self basicNew initialize
%

category: 'queries'
classmethod: GsTestSuiteSample
querySuiteSamples: queryString
	"
		GsTestSuiteSample SuiteSamples.

		(GsTestSuiteSample querySuiteSamples: '(each.branch = ''issue_308'') & (each.gsVersion = ''3.5.0'')') asArray.
	"

	^ GsQuery fromString: queryString on: self suiteSamples
%

category: 'accessing'
classmethod: GsTestSuiteSample
resetSuiteSamples
	"
		GsTestSuiteSample resetSuiteSamples
	"

	SuiteSamples ifNotNil: [ SuiteSamples removeAllIndexes ].
	SuiteSamples := nil
%

category: 'example'
classmethod: GsTestSuiteSample
saveSample: suiteSample
	self suiteSamples add: suiteSample
%

category: 'example'
classmethod: GsTestSuiteSample
saveSampleFromTestSuite: suite testResult: res
	self saveSample: (self generateSampleFromTestSuite: suite testResult: res)
%

category: 'accessing'
classmethod: GsTestSuiteSample
suiteSamples
	"
		GsTestSuiteSample suiteSamples add: 
			(GsTestSuiteSample fromJson: '/home/dhenrich/rogue/_homes/rogue/_home/server/stones/test_rowan_dev_350/testResults.json')
	"

	^ SuiteSamples
		ifNil: [ 
			SuiteSamples := RcLowMaintenanceIdentityBag new.
			self suiteSamplesIndexSpec createIndexesOn: SuiteSamples.
			SuiteSamples ]
%

category: 'indexing'
classmethod: GsTestSuiteSample
suiteSamplesIndexSpec
	| indexOptions |
	indexOptions := GsIndexOptions default + GsIndexOptions reducedConflict.
	^ GsIndexSpec new
		stringOptimizedIndex: 'each.suiteName' options: indexOptions;
		stringOptimizedIndex: 'each.gsVersion' options: indexOptions;
		stringOptimizedIndex: 'each.branch' options: indexOptions;
		stringOptimizedIndex: 'each.commitSha' options: indexOptions;
		equalityIndex: 'each.timeStamp'
			lastElementClass: DateAndTime
			options: indexOptions yourself
%

category: 'example'
classmethod: GsTestSuiteSample
summarizeTestResultsForSuite: suite testResults: res on: strm
	strm
		nextPutAll:
				suite name , ' for GemStone ' , (System gemVersionAt: #'gsVersion') printString;
		lf.

	strm
		nextPutAll: res printString;
		lf.
	strm
		nextPutAll: '  errors';
		lf.
	(res errors collect: [ :each | each printString ]) asArray sort
		do: [ :each | 
			strm
				tab;
				nextPutAll: each;
				lf ].
	res failures size > 0
		ifTrue: [ 
			strm
				nextPutAll: '  failures';
				lf.
			(res failures collect: [ :each | each printString ]) asArray sort
				do: [ :each | 
					strm
						tab;
						nextPutAll: each;
						lf ] ]
%

!		Instance methods for 'GsTestSuiteSample'

category: 'updating'
method: GsTestSuiteSample
addTestCaseSample: aGsTestCaseSample
	self testCases add: aGsTestCaseSample
%

category: 'accessing'
method: GsTestSuiteSample
branch
	^ branch
%

category: 'accessing'
method: GsTestSuiteSample
branch: aString
	branch := aString
%

category: 'accessing'
method: GsTestSuiteSample
commitSha
	^ commitSha
%

category: 'accessing'
method: GsTestSuiteSample
commitSha: aString
	commitSha := aString
%

category: 'comparison'
method: GsTestSuiteSample
compareSelectorsIn: myDict for: theClassName against: theirDict status: status theirSample: theirSuiteSample on: aWriteStream
	| mySortedSelectors theirSortedSelectors mySelectorIndex theirSelectorIndex mySelectorsSize theirSelectorsSize symbol illegalSelector formerStatusIndicator |
	formerStatusIndicator := Dictionary new
		at: #'passed' put: 'P';
		at: #'failures' put: 'F';
		at: #'errors' put: 'E';
		yourself.
	illegalSelector := '' asSymbol.
	mySortedSelectors := myDict at: theClassName ifAbsent: [ #() ].
	theirSortedSelectors := theirDict at: theClassName ifAbsent: [ #() ].
	mySortedSelectors asArray = theirSortedSelectors asArray
		ifTrue: [ 
			"no diffs"
			^ true ].
	mySelectorIndex := 0.
	theirSelectorIndex := 0.
	mySelectorsSize := mySortedSelectors size.
	theirSelectorsSize := theirSortedSelectors size.
	[ 
	mySelectorIndex + 1 <= mySelectorsSize
		or: [ theirSelectorIndex + 1 <= theirSelectorsSize ] ]
		whileTrue: [ 
			| mySelector theirSelector |
			mySelector := mySortedSelectors atOrNil: mySelectorIndex + 1.
			mySelector ifNil: [ mySelector := illegalSelector ].
			theirSelector := theirSortedSelectors atOrNil: theirSelectorIndex + 1.
			theirSelector ifNil: [ theirSelector := illegalSelector ].
			(mySelector _unicodeEqual: theirSelector)
				ifTrue: [ 
					"selector present in both lists ... go on to next selector"
					mySelectorIndex := mySelectorIndex + 1.
					theirSelectorIndex := theirSelectorIndex + 1 ]
				ifFalse: [ 
					((mySelector ~~ illegalSelector and: [ theirSelector == illegalSelector ])
						or: [ 
							mySelector ~~ illegalSelector
								and: [ mySelector _unicodeLessThan: theirSelector ] ])
						ifTrue: [ 
							| res |
							"mySelector present in my list, not present in their list - 
								query to find out if the test exists in their suite of tests"
							res := (theirSuiteSample
								queryTestCases:
									'(each.className = ''' , theClassName , ''') & (each.selector = #'''
										, mySelector , ''')') asArray.
							res size = 0
								ifTrue: [ 
									"not present in their test suite-- new test"
									symbol := ' ++	' ]
								ifFalse: [ 
									"present in their test suite - different status"
									symbol := ' +' , (formerStatusIndicator at: res first status) , '	' ].
							aWriteStream
								nextPutAll: symbol , theClassName , ' debug: #' , mySelector;
								lf.
							mySelectorIndex := mySelectorIndex + 1 ]
						ifFalse: [ 
							"theirSelector not present in my list, present in their list - 
								query to find out if the test exists in my suite of tests"
							theirSelector ~~ illegalSelector
								ifTrue: [ 
									(self
										queryTestCases:
											'(each.className = ''' , theClassName , ''') & (each.selector = #'''
												, theirSelector , ''')') size = 0
										ifTrue: [ 
											| res |
											"not present in my suite of tests -- removed test"
											res := (theirSuiteSample
												queryTestCases:
													'(each.className = ''' , theClassName , ''') & (each.selector = #'''
														, theirSelector , ''')') asArray.
											aWriteStream
												nextPutAll:
														' -' , (formerStatusIndicator at: res first status) , '	' , theClassName , ' debug: #'
																, theirSelector;
												lf ] ].
							theirSelectorIndex := theirSelectorIndex + 1 ] ] ].
	^ false
%

category: 'comparison'
method: GsTestSuiteSample
compareTo: testSuiteSample on: aWriteStream
	"answer the differences for passed, errors, and failures between the receiver and <testSuiteSample>"

	| mySampleMaps theirSampleMaps labels noDiffs statusArray |
	mySampleMaps := self _testCaseSampleMaps.
	theirSampleMaps := testSuiteSample _testCaseSampleMaps.
	labels := {'PASSES: tests: + now passing;  ++ new passing test; - removed test [EFP is indication of previous test status]'.
	'FAILURES: tests: + now failing;  ++ new failing test; - removed test [EFP is indication of previous test status]'.
	'ERRORS: tests: + now erroring;  ++ new erroring test; - removed test [EFP is indication of previous test status]'}.
	statusArray := {#'passed'.
	#'failures'.
	#'errors'}.
	noDiffs := true.
	1 to: 3 do: [ :index | 
		| label myDict theirDict myKeys theirKeys myKey theirKey myKeyIndex theirKeyIndex myKeysSize theirKeysSize |
		label := labels at: index.
		aWriteStream
			nextPutAll: 'Diffs for ' , label;
			lf.
		myDict := mySampleMaps at: index.
		theirDict := theirSampleMaps at: index.
		myKeys := myDict keys asArray sort.
		theirKeys := theirDict keys asArray sort.
		myKeyIndex := 1.
		theirKeyIndex := 1.
		myKeysSize := myKeys size.
		theirKeysSize := theirKeys size.
		[ myKeyIndex <= myKeysSize or: [ theirKeyIndex <= theirKeysSize ] ]
			whileTrue: [ 
				myKey := myKeys atOrNil: myKeyIndex.
				theirKey := theirKeys atOrNil: theirKeyIndex.
				(myKey isNil or: [ theirKey isNil ])
					ifTrue: [ 
						myKey
							ifNil: [ 
								(self
									compareSelectorsIn: myDict
									for: theirKey
									against: theirDict
									status: (statusArray at: index)
									theirSample: testSuiteSample
									on: aWriteStream)
									ifFalse: [ noDiffs := false ].
								theirKeyIndex := theirKeyIndex + 1 ].
						theirKey
							ifNil: [ 
								(self
									compareSelectorsIn: myDict
									for: myKey
									against: theirDict
									status: (statusArray at: index)
									theirSample: testSuiteSample
									on: aWriteStream)
									ifFalse: [ noDiffs := false ].
								myKeyIndex := myKeyIndex + 1 ] ]
					ifFalse: [ 
						(myKey _unicodeEqual: theirKey)
							ifTrue: [ 
								(self
									compareSelectorsIn: myDict
									for: myKey
									against: theirDict
									status: (statusArray at: index)
									theirSample: testSuiteSample
									on: aWriteStream)
									ifFalse: [ noDiffs := false ].
								myKeyIndex := myKeyIndex + 1.
								theirKeyIndex := theirKeyIndex + 1 ]
							ifFalse: [ 
								(myKey _unicodeLessThan: theirKey)
									ifTrue: [ 
										(self
											compareSelectorsIn: myDict
											for: myKey
											against: theirDict
											status: (statusArray at: index)
											theirSample: testSuiteSample
											on: aWriteStream)
											ifFalse: [ noDiffs := false ].
										myKeyIndex := myKeyIndex + 1 ]
									ifFalse: [ 
										(self
											compareSelectorsIn: myDict
											for: theirKey
											against: theirDict
											status: (statusArray at: index)
											theirSample: testSuiteSample
											on: aWriteStream)
											ifFalse: [ noDiffs := false ].
										theirKeyIndex := theirKeyIndex + 1 ] ] ] ] ].
	^ noDiffs
%

category: 'accessing'
method: GsTestSuiteSample
deprecationWarnings
	^ deprecationWarnings ifNil: [ deprecationWarnings := {} ]
%

category: 'accessing'
method: GsTestSuiteSample
deprecationWarnings: aCollection
	deprecationWarnings := aCollection asArray
%

category: 'exporting'
method: GsTestSuiteSample
exportJsonTo: aStream
	| jsonObject jsonTestCases |
	jsonTestCases := {}.
	jsonObject := GsTonelOrderedDictionary new
		at: 'branch' put: self branch;
		at: 'commitSha' put: self commitSha;
		at: 'deprecationWarnings' put: self deprecationWarnings;
		at: 'gsVersion' put: self gsVersion;
		at: 'notes' put: self notes;
		at: 'properties' put: self properties;
		at: 'resultsSummary' put: self resultsSummary;
		at: 'suiteName' put: self suiteName;
		at: 'testCases' put: jsonTestCases;
		at: 'timeStamp' put: self timeStamp printString;
		yourself.
	(self testCases
		sort: [ :a :b | 
			a className < b className
				or: [ a className = b className and: [ a selector <= b selector ] ] ])
		do: [ :testCase | 
			jsonTestCases
				add:
					(GsTonelOrderedDictionary new
						at: 'className' put: testCase className asString;
						at: 'status' put: testCase status asString;
						at: 'selector' put: testCase selector asString;
						at: 'time' put: testCase time asString;
						yourself) ].
	STON put: jsonObject asJsonOnStreamPretty: aStream
%

category: 'accessing'
method: GsTestSuiteSample
gsVersion
	^ gsVersion
%

category: 'accessing'
method: GsTestSuiteSample
gsVersion: aString
	gsVersion := aString
%

category: 'initialization'
method: GsTestSuiteSample
initialize
	branch := commitSha := ''
%

category: 'accessing'
method: GsTestSuiteSample
notes
	^ notes
%

category: 'accessing'
method: GsTestSuiteSample
notes: aString
	notes := aString
%

category: 'enumeration'
method: GsTestSuiteSample
passed: passedBlock failed: failedBlock errored: erroredBlock
	self testCases
		do: [ :testCaseSample | testCaseSample passed: passedBlock failed: failedBlock errored: erroredBlock ]
%

category: 'copying'
method: GsTestSuiteSample
postCopy
	resultsSummary := resultsSummary copy.
	testCases := testCases
		collect: [ :each | 
			| copy |
			copy := each copy.
			copy suite: self.
			copy ].
	deprecationWarnings := deprecationWarnings collect: [ :each | each copy ]
%

category: 'printing'
method: GsTestSuiteSample
printOn: aStream 
	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self suiteName printString, ' ', self gsVersion, ' ' , self branch, ' ', self commitSha, ' ', self timeStamp printString printString;
		nextPut: $)
%

category: 'accessing'
method: GsTestSuiteSample
properties
	^ properties
%

category: 'accessing'
method: GsTestSuiteSample
properties: aDictionary
	properties := aDictionary
%

category: 'queries'
method: GsTestSuiteSample
queryTestCases: queryString
	"
		status: #passed, #failures, #errors

		((GsTestSuiteSample suiteSamples detect: [:each | true ]) 
			queryTestCases: 'each.status == #passed' ) size

		((GsTestSuiteSample suiteSamples detect: [:each | true ]) 
			queryTestCases: 'each.className = ''RwProjectTonelReaderWriterTest''' ) size

		((GsTestSuiteSample suiteSamples detect: [:each | true ]) 
			queryTestCases: '(each.className = ''RwProjectTonelReaderWriterTest'') & (each.status ~~ #passed)' ) size

	"

	^ GsQuery fromString: queryString on: self testCases
%

category: 'accessing'
method: GsTestSuiteSample
resultsSummary
	^ resultsSummary ifNil: [ resultsSummary := Dictionary new ]
%

category: 'accessing'
method: GsTestSuiteSample
resultsSummary: aDictionary
	resultsSummary := aDictionary
%

category: 'accessing'
method: GsTestSuiteSample
suiteName
	^ suiteName
%

category: 'accessing'
method: GsTestSuiteSample
suiteName: aString
	suiteName := aString
%

category: 'printing'
method: GsTestSuiteSample
suiteSummary
	| strm |
	strm := WriteStream on: String new.
	self suiteSummaryOn: strm.
	^ strm contents
%

category: 'printing'
method: GsTestSuiteSample
suiteSummaryOn: strm
	| passed failed errored |
	self printOn: strm.
	strm lf.
	strm
		nextPutAll: (self resultsSummary at: 'summary');
		lf.
	passed := {}.
	failed := {}.
	errored := {}.
	self
		passed: [ :each | passed add: each ]
		failed: [ :each | failed add: each ]
		errored: [ :each | errored add: each ].

	strm
		nextPutAll: '  errors';
		lf.
	(errored collect: [ :each | each printString ]) asArray sort
		do: [ :each | 
			strm
				tab;
				nextPutAll: each;
				lf ].
	failed size > 0
		ifTrue: [ 
			strm
				nextPutAll: '  failures';
				lf.
			(failed collect: [ :each | each printString ]) asArray sort
				do: [ :each | 
					strm
						tab;
						nextPutAll: each;
						lf ] ]
%

category: 'accessing'
method: GsTestSuiteSample
testCases
	^ testCases
		ifNil: [ 
			testCases := RcLowMaintenanceIdentityBag new.
			self testCaseSampleIndexSpec createIndexesOn: testCases.
			testCases ]
%

category: 'accessing'
method: GsTestSuiteSample
testCases: anArrayOfTestCaseSampleDictionaries
	anArrayOfTestCaseSampleDictionaries
		do: [ :sampleDict | 
			| testCaseSample |
			testCaseSample := GsTestCaseSample fromDictionary: sampleDict.
			testCaseSample suite: self.
			self testCases add: testCaseSample ]
%

category: 'indexing'
method: GsTestSuiteSample
testCaseSampleIndexSpec
	| indexOptions |
	indexOptions := GsIndexOptions default + GsIndexOptions reducedConflict.
	^ GsIndexSpec new
		stringOptimizedIndex: 'each.className' options: indexOptions;
		symbolOptimizedIndex: 'each.selector' options: indexOptions;
		symbolOptimizedIndex: 'each.status' options: indexOptions;
		yourself
%

category: 'accessing'
method: GsTestSuiteSample
timeStamp
	^ timeStamp
%

category: 'accessing'
method: GsTestSuiteSample
timeStamp: aDateAndTimeOrString
	timeStamp := aDateAndTimeOrString isString
		ifTrue: [ DateAndTime fromString: aDateAndTimeOrString ]
		ifFalse: [ aDateAndTimeOrString ]
%

category: 'accessing'
method: GsTestSuiteSample
_testCases: aCollectionOfTestCaseSamples
	"replace existing test cases with a new collection of test cases"

	testCases := nil.
	aCollectionOfTestCaseSamples
		do: [ :testCaseSample | self testCases add: testCaseSample ]
%

category: 'comparison'
method: GsTestSuiteSample
_testCaseSampleMaps
	| pass fail error |
	pass := Dictionary new.
	fail := Dictionary new.
	error := Dictionary new.
	(self queryTestCases: 'each.status == #passed')
		do: [ :testCaseSample | 
			(pass at: testCaseSample className ifAbsentPut: [ IcuSortedCollection new ])
				add: testCaseSample selector ].
	(self queryTestCases: 'each.status == #failures')
		do: [ :testCaseSample | 
			(fail at: testCaseSample className ifAbsentPut: [ IcuSortedCollection new ])
				add: testCaseSample selector ].
	(self queryTestCases: 'each.status == #errors')
		do: [ :testCaseSample | 
			(error at: testCaseSample className ifAbsentPut: [ IcuSortedCollection new ])
				add: testCaseSample selector ].
	^ { pass . fail . error}
%

category: 'comparison'
method: GsTestSuiteSample
_testCasesMap
	| classMap |
	classMap := Dictionary new.
	self testCases
		do: [ :testCaseSample | 
			| selectorMap |
			selectorMap := classMap
				at: testCaseSample className asSymbol
				ifAbsentPut: [ Dictionary new ].
			selectorMap at: testCaseSample selector asSymbol put: testCaseSample ].
	^ classMap
%

! Class extensions for 'Behavior'

!		Instance methods for 'Behavior'

category: '*gsteststats-kernel-extensions'
method: Behavior
includesBehavior: aClass
	^ self == aClass or: [ self inheritsFrom: aClass ]
%

! Class extensions for 'Character'

!		Class methods for 'Character'

category: '*gsteststats-kernel-extensions'
classmethod: Character
escape
	^ self esc
%

! Class extensions for 'CharacterCollection'

!		Instance methods for 'CharacterCollection'

category: '*gsteststats-kernel-extensions'
method: CharacterCollection
contractTo: smallSize
	"return myself or a copy shortened by ellipsis to smallSize"

	"
		'A clear but rather long-winded summary' contractTo: 18
	"

	| leftSize mySize |
	(mySize := self size) <= smallSize
		ifTrue: [ ^ self ].	"short enough"
	smallSize < 5
		ifTrue: [ ^ self copyFrom: 1 to: smallSize ].	"First N characters"
	leftSize := (smallSize - 2) // 2.
	^ self
		copyReplaceFrom: leftSize + 1
		to: mySize - (smallSize - leftSize - 3)
		with: '...'
%

category: '*gsteststats-kernel-extensions'
method: CharacterCollection
truncateTo: smallSize
	"return myself or a copy shortened to smallSize.  1/18/96 sw"

	^ self size <= smallSize
		ifTrue: [ self ]
		ifFalse: [ self copyFrom: 1 to: smallSize ]
%

! Class extensions for 'TestCase'

!		Class methods for 'TestCase'

category: '*gsteststats-kernel-extensions'
classmethod: TestCase
addToSuite: suite fromMethods: testMethods
	testMethods do: [ :selector | suite addTest: (self selector: selector) ].
	^ suite
%

category: '*gsteststats-kernel-extensions'
classmethod: TestCase
addToSuiteFromSelectors: suite
	^ self
		addToSuite: suite
		fromMethods:
			(self shouldInheritSelectors
				ifTrue: [ self allTestSelectors ]
				ifFalse: [ self testSelectors ])
%

!		Instance methods for 'TestCase'

category: '*gsteststats-kernel-extensions'
method: TestCase
debug: aResult
	aResult debugCase: self
%

category: '*gsteststats-kernel-extensions'
method: TestCase
fail: aDescriptionString
	^ self assert: false description: aDescriptionString
%

category: '*gsteststats-kernel-extensions'
method: TestCase
shouldPass
	"expected failures not supported at the moment, so all tests should pass"

	^ true
%

