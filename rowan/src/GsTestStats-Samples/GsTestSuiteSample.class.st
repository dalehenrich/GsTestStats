Class {
	#name : 'GsTestSuiteSample',
	#superclass : 'Object',
	#instVars : [
		'suiteName',
		'properties',
		'testCases',
		'resultsSummary',
		'timeStamp',
		'notes',
		'gsVersion',
		'branch',
		'commitSha'
	],
	#classVars : [
		'SuiteSamples'
	],
	#category : 'GsTestStats-Samples'
}

{ #category : 'comparison' }
GsTestSuiteSample class >> compareLatestTestCasesFor: aGsVersion branch: aBranch to: testSuiteSample on: aWriteStream [
	| latestTestSuiteSample |
	latestTestSuiteSample := self
		latestQuerySuiteSampleFor: aBranch
		andVersion: aGsVersion.
	^ testSuiteSample compareTo: latestTestSuiteSample on: aWriteStream
]

{ #category : 'instance creation' }
GsTestSuiteSample class >> fromDictionary: sampleDict [

	| newSuiteSample |
	newSuiteSample := self new.
	sampleDict keysAndValuesDo: [:key :value |
		newSuiteSample perform: (key, ':') with: value ].
	^ newSuiteSample
]

{ #category : 'instance creation' }
GsTestSuiteSample class >> fromJson: filePath [
	| fileRef sampleDict |
	fileRef := filePath asFileReference.
	fileRef
		readStreamDo: [ :fileStream | 
			| bufferedStream |
			bufferedStream := ZnBufferedReadStream on: fileStream.
			bufferedStream sizeBuffer: fileRef size.
			sampleDict := STON fromStream: bufferedStream ].
	^ self fromDictionary: sampleDict
]

{ #category : 'instance creation' }
GsTestSuiteSample class >> fromJsonStream: aStream [
	| sample resultsSummary |
	sample := self fromDictionary: (STON fromStream: aStream).
	resultsSummary := sample resultsSummary.
	resultsSummary copy
		keysAndValuesDo: [ :key :value | 
			"repair results summary for now"
			resultsSummary at: key asSymbol put: value.
			resultsSummary removeKey: key asString ifAbsent: [  ] ].
	^ sample
]

{ #category : 'example' }
GsTestSuiteSample class >> generateResultsDictionarFromTestSuite: suite testResult: res [
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
]

{ #category : 'example' }
GsTestSuiteSample class >> generateSampleFromTestSuite: suite testResult: res [
	^ self
		fromDictionary:
			(self generateResultsDictionarFromTestSuite: suite testResult: res)
]

{ #category : 'queries' }
GsTestSuiteSample class >> latestQuerySuiteSampleFor: branch andVersion: gsVersion [
	"return the latest suite sample for the given <branch> and <gsVersion>. Return nil if no sample satisfies the query"

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
			(sample branch = branch and: [ sample gsVersion = gsVersion ])
				ifTrue: [ ^ sample ] ].
	^ nil
]

{ #category : 'instance creation' }
GsTestSuiteSample class >> new [

	^ self basicNew initialize
]

{ #category : 'queries' }
GsTestSuiteSample class >> querySuiteSamples: queryString [
	"
		GsTestSuiteSample SuiteSamples.

		(GsTestSuiteSample querySuiteSamples: '(each.branch = ''issue_308'') & (each.gsVersion = ''3.5.0'')') asArray.
	"

	^ GsQuery fromString: queryString on: self suiteSamples
]

{ #category : 'accessing' }
GsTestSuiteSample class >> resetSuiteSamples [
	"
		GsTestSuiteSample resetSuiteSamples
	"

	SuiteSamples ifNotNil: [ SuiteSamples removeAllIndexes ].
	SuiteSamples := nil
]

{ #category : 'example' }
GsTestSuiteSample class >> saveSample: suiteSample [
	self suiteSamples add: suiteSample
]

{ #category : 'example' }
GsTestSuiteSample class >> saveSampleFromTestSuite: suite testResult: res [
	self saveSample: (self generateSampleFromTestSuite: suite testResult: res)
]

{ #category : 'accessing' }
GsTestSuiteSample class >> suiteSamples [
	"
		GsTestSuiteSample suiteSamples add: 
			(GsTestSuiteSample fromJson: '/home/dhenrich/rogue/_homes/rogue/_home/server/stones/test_rowan_dev_350/testResults.json')
	"

	^ SuiteSamples
		ifNil: [ 
			SuiteSamples := RcLowMaintenanceIdentityBag new.
			self suiteSamplesIndexSpec createIndexesOn: SuiteSamples.
			SuiteSamples ]
]

{ #category : 'indexing' }
GsTestSuiteSample class >> suiteSamplesIndexSpec [
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
]

{ #category : 'example' }
GsTestSuiteSample class >> summarizeTestResultsForSuite: suite testResults: res on: strm [
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
]

{ #category : 'accessing' }
GsTestSuiteSample >> _testCases: aCollectionOfTestCaseSamples [
	"replace existing test cases with a new collection of test cases"

	testCases := nil.
	aCollectionOfTestCaseSamples
		do: [ :testCaseSample | self testCases add: testCaseSample ]
]

{ #category : 'comparison' }
GsTestSuiteSample >> _testCaseSampleMaps [
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
]

{ #category : 'accessing' }
GsTestSuiteSample >> branch [
	^ branch
]

{ #category : 'accessing' }
GsTestSuiteSample >> branch: aString [
	branch := aString
]

{ #category : 'accessing' }
GsTestSuiteSample >> commitSha [
	^ commitSha
]

{ #category : 'accessing' }
GsTestSuiteSample >> commitSha: aString [
	commitSha := aString
]

{ #category : 'comparison' }
GsTestSuiteSample >> compareSelectorsIn: myDict for: theClassName against: theirDict status: status theirSample: theirSuiteSample on: aWriteStream [
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
]

{ #category : 'comparison' }
GsTestSuiteSample >> compareTo: testSuiteSample on: aWriteStream [
	"answer the differences for passed, errors, and failures between the receiver and <testSuiteSample>"

	| mySampleMaps theirSampleMaps labels noDiffs statusArray |
	mySampleMaps := self _testCaseSampleMaps.
	theirSampleMaps := testSuiteSample _testCaseSampleMaps.
	labels := {'PASSES: tests: + now passing;  ++ new passing test; - removed test [EFP is indication of previous test status]'.
	'FAILURES: tests: + now failing;  ++ new failing test; - removed test [EFP is indication of previous test status]'.
	'ERRORS: tests: + now erroring;  ++ new erroring test; - removed test [EFP is indication of previous test status]'}.
	statusArray := {#'passing'.
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
]

{ #category : 'accessing' }
GsTestSuiteSample >> gsVersion [
	^ gsVersion
]

{ #category : 'accessing' }
GsTestSuiteSample >> gsVersion: aString [
	gsVersion := aString
]

{ #category : 'initialization' }
GsTestSuiteSample >> initialize [
	branch := commitSha := ''
]

{ #category : 'accessing' }
GsTestSuiteSample >> notes [
	^ notes
]

{ #category : 'accessing' }
GsTestSuiteSample >> notes: aString [
	notes := aString
]

{ #category : 'enumeration' }
GsTestSuiteSample >> passed: passedBlock failed: failedBlock errored: erroredBlock [
	self testCases
		do: [ :testCaseSample | testCaseSample passed: passedBlock failed: failedBlock errored: erroredBlock ]
]

{ #category : 'printing' }
GsTestSuiteSample >> printOn: aStream [ 
	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self suiteName printString, ' ', self gsVersion, ' ' , self branch, ' ', self commitSha, ' ', self timeStamp printString printString;
		nextPut: $)
]

{ #category : 'accessing' }
GsTestSuiteSample >> properties [
	^ properties
]

{ #category : 'accessing' }
GsTestSuiteSample >> properties: aDictionary [
	properties := aDictionary
]

{ #category : 'queries' }
GsTestSuiteSample >> queryTestCases: queryString [
	"
		status: #passed, #failure, #error

		((GsTestSuiteSample suiteSamples detect: [:each | true ]) 
			queryTestCases: 'each.status == #passed' ) size

		((GsTestSuiteSample suiteSamples detect: [:each | true ]) 
			queryTestCases: 'each.className = ''RwProjectTonelReaderWriterTest''' ) size

		((GsTestSuiteSample suiteSamples detect: [:each | true ]) 
			queryTestCases: '(each.className = ''RwProjectTonelReaderWriterTest'') & (each.status ~~ #passed)' ) size

	"

	^ GsQuery fromString: queryString on: self testCases
]

{ #category : 'accessing' }
GsTestSuiteSample >> resultsSummary [
	^ resultsSummary
]

{ #category : 'accessing' }
GsTestSuiteSample >> resultsSummary: aDictionary [
	resultsSummary := aDictionary
]

{ #category : 'accessing' }
GsTestSuiteSample >> suiteName [
	^ suiteName
]

{ #category : 'accessing' }
GsTestSuiteSample >> suiteName: aString [
	suiteName := aString
]

{ #category : 'printing' }
GsTestSuiteSample >> suiteSummary [
	| strm |
	strm := WriteStream on: String new.
	self suiteSummaryOn: strm.
	^ strm contents
]

{ #category : 'printing' }
GsTestSuiteSample >> suiteSummaryOn: strm [
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
]

{ #category : 'accessing' }
GsTestSuiteSample >> testCases [
	^ testCases
		ifNil: [ 
			testCases := RcLowMaintenanceIdentityBag new.
			self testCaseSampleIndexSpec createIndexesOn: testCases.
			testCases ]
]

{ #category : 'accessing' }
GsTestSuiteSample >> testCases: anArrayOfTestCaseSampleDictionaries [
	anArrayOfTestCaseSampleDictionaries
		do: [ :sampleDict | 
			| testCaseSample |
			testCaseSample := GsTestCaseSample fromDictionary: sampleDict.
			testCaseSample suite: self.
			self testCases add: testCaseSample ]
]

{ #category : 'indexing' }
GsTestSuiteSample >> testCaseSampleIndexSpec [
	| indexOptions |
	indexOptions := GsIndexOptions default + GsIndexOptions reducedConflict.
	^ GsIndexSpec new
		stringOptimizedIndex: 'each.className' options: indexOptions;
		symbolOptimizedIndex: 'each.selector' options: indexOptions;
		symbolOptimizedIndex: 'each.status' options: indexOptions;
		yourself
]

{ #category : 'accessing' }
GsTestSuiteSample >> timeStamp [
	^ timeStamp
]

{ #category : 'accessing' }
GsTestSuiteSample >> timeStamp: aDateAndTimeOrString [
	timeStamp := aDateAndTimeOrString isString
		ifTrue: [ DateAndTime fromString: aDateAndTimeOrString ]
		ifFalse: [ aDateAndTimeOrString ]
]
