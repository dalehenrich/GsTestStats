Class {
	#name : 'SmalltalkCISpec',
	#superclass : 'Object',
	#instVars : [
		'loading',
		'testing',
		'configuring',
		'preLoading',
		'postLoading',
		'preTesting',
		'postTesting',
		'name',
		'specName'
	],
	#category : 'GsTestStats-SmalltalkCI-TestSupport'
}

{ #category : 'defaults' }
SmalltalkCISpec class >> defaultName [
	^ GsTestStatsCI smalltalkSelection ifNil: [ 'unknown' ]
]

{ #category : 'instance creation' }
SmalltalkCISpec class >> fromFile: aFilename [
	GsTestStatsCI readOnlyFileNamed: aFilename
		do: [ :stream | ^ self fromStream: stream]
]

{ #category : 'instance creation' }
SmalltalkCISpec class >> fromStream: aStream [
	| result |
	^ [ result := STON fromStream: aStream.
		(result isKindOf: self)
			ifTrue: [ result ]
			ifFalse: [ SmalltalkCIInvalidSpec new ]]
				on: STONReaderError
					do: [ :e | SmalltalkCIInvalidSpec withError: e ]
]

{ #category : 'instance creation' }
SmalltalkCISpec class >> writeOn: aStream [
	(STON writer on: aStream)
		prettyPrint: true;
		jsonMode: false;
		referencePolicy: #error;
		nextPut: self
]

{ #category : 'accessing' }
SmalltalkCISpec >> addLoadSpec: aLoadSpec [
	aLoadSpec isComplete ifTrue: [ self loading add: aLoadSpec ]
]

{ #category : 'helpers' }
SmalltalkCISpec >> classesToTest [
	| classes |
	GsTestStatsCI explicitTestKeys
		do: [ :key | 
			(self testing includesKey: key)
				ifTrue: [ ^ GsTestStatsCI classesFrom: self testing ] ].
	classes := OrderedCollection new.
	(self testing at: #'allTestCases' ifAbsent: [ false ]) = true
		ifTrue: [ classes addAll: TestCase allSubclasses ]
		ifFalse: [ "By default, add all classes that have been loaded by compatible LoadSpecs"
			self compatibleLoadSpecs do: [ :each | classes addAll: each loadedClasses ] ].
	self testing at: #'include'
		ifPresent: [ :includeDict | classes addAll: (GsTestStatsCI classesFrom: includeDict) ].
	self testing at: #'exclude'
 		ifPresent: [ :excludeDict | classes removeAllFoundIn: (GsTestStatsCI classesFrom: excludeDict) ].
	^ classes select: [ :cl | cl inheritsFrom: TestCase ]

]

{ #category : 'helpers' }
SmalltalkCISpec >> compatibleConfigurationsFor: aPlatform [
	^ self configuring select: [ :each | each isPlatformCompatible: aPlatform ]
]

{ #category : 'helpers' }
SmalltalkCISpec >> compatibleCustomScripts: aPhase [
	^ (self customScripts: aPhase) select: [ :each | each isPlatformCompatible ]
]

{ #category : 'helpers' }
SmalltalkCISpec >> compatibleLoadSpecs [
	^ self loading select: [ :each | each isPlatformCompatible ]
]

{ #category : 'accessing' }
SmalltalkCISpec >> configuring [
	^ configuring ifNil: [ configuring := OrderedCollection new ]
]

{ #category : 'accessing' }
SmalltalkCISpec >> configuring: anObject [
	configuring := anObject
]

{ #category : 'helpers' }
SmalltalkCISpec >> coverageDictionary [
	^ self testing at: #coverage ifAbsent: [ Dictionary new ]
]

{ #category : 'helpers' }
SmalltalkCISpec >> coverageEnabled [
	^ self testing includesKey: #coverage
]

{ #category : 'helpers' }
SmalltalkCISpec >> customScripts: aPhase [
	| input |
	input := self instVarNamed: aPhase.
	input ifNil: [ ^ #() ].
	(input isKindOf: (Rowan globalNamed: 'SCICustomScript')) ifTrue: [ ^ {input} ].
	input isString ifTrue: [ ^ { (Rowan globalNamed: 'SCICustomScript') newForPath: input } ].
	input isCollection ifTrue: [
		| scripts |
		scripts := OrderedCollection new.
		input do: [ :each |
			(each isKindOf: (Rowan globalNamed: 'SCICustomScript')) ifTrue: [ scripts add: each ].
			each isString ifTrue: [ scripts add: ((Rowan globalNamed: 'SCICustomScript') newForPath: each) ]].
		^ scripts ].
	self error: 'Invalid input: ', input asString
]

{ #category : 'helpers' }
SmalltalkCISpec >> failOnSCIDeprecationWarnings [
	^ self testing at: #failOnSCIDeprecationWarnings ifAbsent: [ false ]
]

{ #category : 'helpers' }
SmalltalkCISpec >> failOnZeroTests [
	^ self testing at: #failOnZeroTests ifAbsent: [ true ]
]

{ #category : 'helpers' }
SmalltalkCISpec >> hidePassingTests [
	^ self testing at: #hidePassingTests ifAbsent: [ false ]
]

{ #category : 'helpers' }
SmalltalkCISpec >> isValid [
	^ true
]

{ #category : 'accessing' }
SmalltalkCISpec >> loading [
	^ loading ifNil: [ loading := OrderedCollection new ]
]

{ #category : 'accessing' }
SmalltalkCISpec >> loading: loadSpecifications [
	loading := loadSpecifications
]

{ #category : 'accessing' }
SmalltalkCISpec >> name [
	^ name ifNil: [
		specName ifNotNil: [
			GsTestStatsCI deprecated: 'Use #name instead of #specName'.
			^ specName ].
		self class defaultName ]
]

{ #category : 'accessing' }
SmalltalkCISpec >> name: aName [

	name := aName
]

{ #category : 'accessing' }
SmalltalkCISpec >> postLoading [

	^ postLoading
]

{ #category : 'accessing' }
SmalltalkCISpec >> postLoading: anObject [

	postLoading := anObject
]

{ #category : 'accessing' }
SmalltalkCISpec >> postTesting [

	^ postTesting
]

{ #category : 'accessing' }
SmalltalkCISpec >> postTesting: anObject [

	postTesting := anObject
]

{ #category : 'accessing' }
SmalltalkCISpec >> preLoading [

	^ preLoading
]

{ #category : 'accessing' }
SmalltalkCISpec >> preLoading: anObject [

	preLoading := anObject
]

{ #category : 'accessing' }
SmalltalkCISpec >> preTesting [

	^ preTesting
]

{ #category : 'accessing' }
SmalltalkCISpec >> preTesting: anObject [

	preTesting := anObject
]

{ #category : 'accessing' }
SmalltalkCISpec >> testing [
	^ testing ifNil: [ testing := Dictionary new ]
]

{ #category : 'accessing' }
SmalltalkCISpec >> testing: aDictionary [
	testing := aDictionary
]
