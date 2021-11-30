Class {
	#name : 'GsTestStatsCITest',
	#superclass : 'TestCase',
	#instVars : [
		'sci'
	],
	#category : 'GsTestStats-SmalltalkCI-TestSupport-Tests'
}

{ #category : 'helpers' }
GsTestStatsCITest class >> expectedTests: aClass [
	^ (GsTestStatsCI methodDictOf: aClass) keys select: [ :ea | ea beginsWith: 'test' ]
]

{ #category : 'accessing' }
GsTestStatsCITest >> sci [

	^ sci
]

{ #category : 'accessing' }
GsTestStatsCITest >> sci: anObject [

	sci := anObject
]

{ #category : 'setup' }
GsTestStatsCITest >> setUp [
	self sci: GsTestStatsCI basicNew initialize
]

{ #category : 'testing' }
GsTestStatsCITest >> testAppVeyorDetection [
	(GsTestStatsCI getEnv: 'USER') = 'appveyor'
		ifTrue: [ self assert: GsTestStatsCI isAppVeyorBuild ]
		ifFalse: [ self deny: GsTestStatsCI isAppVeyorBuild ].

	GsTestStatsCI isAppVeyorBuild ifTrue: [
		self
			assert: 'True'
			equals: (GsTestStatsCI getEnv: 'CI').
		self
			assert: (GsTestStatsCI getEnv: 'APPVEYOR_PROJECT_NAME') notEmpty
	]
]

{ #category : 'testing' }
GsTestStatsCITest >> testAuthor [
	| currentAuthor |
	currentAuthor := GsTestStatsCI getAuthor.
	
	[
	GsTestStatsCI basicNew initialize.
	self deny: GsTestStatsCI defaultAuthor isNil.
	] ensure: [
		GsTestStatsCI setAuthor: currentAuthor.
		self assert: GsTestStatsCI getAuthor equals: currentAuthor ]
]

{ #category : 'testing' }
GsTestStatsCITest >> testClassesForCategories [
	| classes size |
	classes := GsTestStatsCI classesForCategories: #('GsTestStatsCI-Core').
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec).
	self deny: (classes includes: GsTestStatsCITest).
	self deny: (classes includes: SmalltalkCISpecTest).
	self assert: classes size equals: (GsTestStatsCI classesInCategory: 'GsTestStatsCI-Core') size.
	
	classes := GsTestStatsCI classesForCategories: #('GsTestStatsCI-Cor.*' 'GsTestStatsCI-T.*').
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec).
	self assert: (classes includes: GsTestStatsCITest).
	self assert: (classes includes: SmalltalkCISpecTest).
	self deny: (classes includes: Object).
	size := (GsTestStatsCI classesInCategory: 'GsTestStatsCI-Core') size.
	size := size + (GsTestStatsCI classesInCategory: 'GsTestStatsCI-Tests') size.
	self assert: classes size equals: size.
]

{ #category : 'testing' }
GsTestStatsCITest >> testClassesForPackages [
	| classes size |
	classes := GsTestStatsCI classesForPackages: #('GsTestStatsCI-Core').
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec).
	self deny: (classes includes: GsTestStatsCITest).
	self deny: (classes includes: SmalltalkCISpecTest).
	self assert: classes size equals: (GsTestStatsCI classesInPackage: 'GsTestStatsCI-Core') size.
	
	classes := GsTestStatsCI classesForPackages: #('GsTestStatsCI-Cor.*' 'GsTestStatsCI-T.*').
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec).
	self assert: (classes includes: GsTestStatsCITest).
	self assert: (classes includes: SmalltalkCISpecTest).
	self deny: (classes includes: Object).
	size := (GsTestStatsCI classesInPackage: 'GsTestStatsCI-Core') size.
	size := size + (GsTestStatsCI classesInPackage: 'GsTestStatsCI-Tests') size.
	self assert: classes size equals: size.
]

{ #category : 'testing' }
GsTestStatsCITest >> testClassesFrom [
	| classes testing |
	testing := (Dictionary new
        add: #'classes' -> #(Dictionary);
        add: #'categories' -> #('GsTestStatsCI-Core');
        yourself).
	classes := GsTestStatsCI classesFrom: testing.
	self assert: (classes includes: Dictionary).
	self assert: (classes includes: GsTestStatsCI).
	self deny: (classes includes: GsTestStatsCITest).
	
	testing := (Dictionary new
        add: #'packages' -> #('GsTestStatsCI-Core');
        yourself).
	classes := GsTestStatsCI classesFrom: testing.
	self deny: (classes includes: Dictionary).
	self assert: (classes includes: GsTestStatsCI).
	self deny: (classes includes: GsTestStatsCITest).

	testing := (Dictionary new
        add: #'packages' -> #('GsTestStatsCI-*');
        yourself).
	classes := GsTestStatsCI classesFrom: testing.
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: GsTestStatsCITest).
	self assert: (classes includes: SCIExcludedTests)
]

{ #category : 'testing' }
GsTestStatsCITest >> testClassesInCategory [
	| classes |
	classes := GsTestStatsCI classesInCategory: 'GsTestStatsCI-Core'.
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec).
	self deny: (classes includes: GsTestStatsCITest).
	self deny: (classes includes: SmalltalkCISpecTest).
]

{ #category : 'testing' }
GsTestStatsCITest >> testClassesInPackage [
	| classes |
	classes := GsTestStatsCI classesInPackage: 'GsTestStatsCI-Core'.
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec).
	self deny: (classes includes: GsTestStatsCITest).
	self deny: (classes includes: SmalltalkCISpecTest)
]

{ #category : 'testing' }
GsTestStatsCITest >> testClassesOfProjects [
	| classes |
	classes := GsTestStatsCI classesOfProjects: #('SomeNonExistingProject').
	self assert: classes size = 0.
	
	classes := GsTestStatsCI classesOfProjects: #('GsTestStatsCI').
	self assert: classes size > 0.
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec)
]

{ #category : 'testing' }
GsTestStatsCITest >> testClassesWithCategoryNames [
	| classes size |
	classes := GsTestStatsCI classesWithCategoryNames: #('GsTestStatsCI-Core').
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec).
	self deny: (classes includes: GsTestStatsCITest).
	self deny: (classes includes: SmalltalkCISpecTest).
	self assert: classes size equals: (GsTestStatsCI classesInCategory: 'GsTestStatsCI-Core') size.
	
	classes := GsTestStatsCI classesWithCategoryNames: #('GsTestStatsCI-Core' 'GsTestStatsCI-Tests').
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec).
	self assert: (classes includes: GsTestStatsCITest).
	self assert: (classes includes: SmalltalkCISpecTest).
	self deny: (classes includes: Object).
	size := (GsTestStatsCI classesInCategory: 'GsTestStatsCI-Core') size.
	size := size + (GsTestStatsCI classesInCategory: 'GsTestStatsCI-Tests') size.
	self assert: classes size equals: size.
]

{ #category : 'testing' }
GsTestStatsCITest >> testClassesWithPackageNames [
	| classes size |
	classes := GsTestStatsCI classesWithPackageNames: #('GsTestStatsCI-Core').
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec).
	self deny: (classes includes: GsTestStatsCITest).
	self deny: (classes includes: SmalltalkCISpecTest).
	self assert: classes size equals: (GsTestStatsCI classesInPackage: 'GsTestStatsCI-Core') size.
	
	classes := GsTestStatsCI classesWithPackageNames: #('GsTestStatsCI-Core' 'GsTestStatsCI-Tests').
	self assert: (classes includes: GsTestStatsCI).
	self assert: (classes includes: SmalltalkCISpec).
	self assert: (classes includes: GsTestStatsCITest).
	self assert: (classes includes: SmalltalkCISpecTest).
	self deny: (classes includes: Object).
	size := (GsTestStatsCI classesInPackage: 'GsTestStatsCI-Core') size.
	size := size + (GsTestStatsCI classesInPackage: 'GsTestStatsCI-Tests') size.
	self assert: classes size equals: size.
]

{ #category : 'testing' }
GsTestStatsCITest >> testGitLabCIDetection [
	(GsTestStatsCI getEnv: 'GITLAB_CI') = 'true'
		ifTrue: [ self assert: GsTestStatsCI isGitLabCIBuild ]
		ifFalse: [ self deny: GsTestStatsCI isGitLabCIBuild ]
]

{ #category : 'testing' }
GsTestStatsCITest >> testNew [
	self should: [ GsTestStatsCI new ] raise: Error
]

{ #category : 'testing' }
GsTestStatsCITest >> testNewTravisID [
	| travisID |
	travisID := GsTestStatsCI platformClass newTravisID.
	self assert: travisID ~= GsTestStatsCI platformClass newTravisID.
	self assert: 4 equals: travisID size

]

{ #category : 'testing' }
GsTestStatsCITest >> testResolveAllWith [
	| list result |
	list := #('foo' 'bar' 'foo-bar' 'bar-foo' 'foo-car').
	self assert: #('foo') equals: (GsTestStatsCI resolveAll: #('foo') with: list).
	self assert: #() equals: (GsTestStatsCI resolveAll: #('zzz') with: list).

	result := GsTestStatsCI resolveAll: #('foo' 'bar') with: list.
	self assert: (result size = 2).
	self assert: (result includes: 'foo').
	self assert: (result includes: 'bar').

	result := GsTestStatsCI resolveAll: #('foo-*' 'bar.*') with: list.
	self assert: (result size = 4).
	self assert: (result includes: 'foo-bar').
	self assert: (result includes: 'foo-car').
	self assert: (result includes: 'bar').
	self assert: (result includes: 'bar-foo').

	result := GsTestStatsCI resolveAll: #('*') with: list.
	self assert: (result size = list size).
	result := GsTestStatsCI resolveAll: #('.*') with: list.
	self assert: (result size = list size).
]

{ #category : 'testing' }
GsTestStatsCITest >> testResolveWith [
	| list result |
	list := #('foo' 'bar' 'foo-bar' 'bar-foo' 'foo-car').
	self assert: #('foo') equals: (GsTestStatsCI resolve: 'foo' with: list).
	self assert: #() equals: (GsTestStatsCI resolve: 'zzz' with: list).

	result := GsTestStatsCI resolve: 'foo-*' with: list.
	self assert: (result size = 2).
	self assert: (result includes: 'foo-bar').
	self assert: (result includes: 'foo-car').
	
	result := GsTestStatsCI resolve: 'foo-.*' with: list.
	self assert: (result size = 2).
	self assert: (result includes: 'foo-bar').
	self assert: (result includes: 'foo-car').

	result := GsTestStatsCI resolve: 'foo.*' with: list.
	self assert: (result size = 3).
	self assert: (result includes: 'foo').
	self assert: (result includes: 'foo-bar').
	self assert: (result includes: 'foo-car').

	result := GsTestStatsCI resolve: '*' with: list.
	self assert: (result size = list size).
	result := GsTestStatsCI resolve: '.*' with: list.
	self assert: (result size = list size).
]

{ #category : 'testing' }
GsTestStatsCITest >> testTravisDetection [
	| path hasJoshsApproval isTravisPath |
	path := GsTestStatsCI platformClass imagePath.
	hasJoshsApproval := (GsTestStatsCI getEnv: 'HAS_JOSH_K_SEAL_OF_APPROVAL') notNil.
	"Linux or macOS path"
	isTravisPath := ((path findString: '/home/travis/') = 1) or: [ (path findString: '/Users/travis/') = 1 ].
	(hasJoshsApproval and: isTravisPath)
		ifTrue: [ self assert: GsTestStatsCI isTravisBuild ]
		ifFalse: [ self deny: GsTestStatsCI isTravisBuild ].

	GsTestStatsCI isTravisBuild ifTrue: [
		self
			assert: 'true'
			equals: (GsTestStatsCI getEnv: 'CI').
		self
			assert: (GsTestStatsCI getEnv: 'TRAVIS_SMALLTALK_VERSION') notEmpty
	]
]

{ #category : 'testing' }
GsTestStatsCITest >> testTravisFold [
	| stream result index |
	stream := WriteStream on: (String new: 1000).
	GsTestStatsCI platformClass travisFold: 'Some Stage Title' id: 'stage_id' on: stream block: [ stream nextPutAll: '@@@' ].
	result := stream contents.
	"Check content and respect order"
	index := result findString: 'travis_fold:start:stage_id'.
	self assert: index = 1.
	index := result findString: 'travis_time:start:stage_id_timer' startingAt: index.
	self assert: index > 0.
	index := result findString: 'Some Stage Title' startingAt: index.
	self assert: index > 0.
	index := result findString: (GsTestStatsCI newLine) startingAt: index.
	self assert: index > 0.
	index := result findString: '@@@' startingAt: index.
	self assert: index > 0.
	index := result findString: 'travis_time:end:stage_id_timer' startingAt: index.
	self assert: index > 0.
	index := result findString: ',duration=' startingAt: index.
	self assert: index > 0.
	index := result findString: 'travis_fold:end:stage_id' startingAt: index.
	self assert: index > 0
	

]
