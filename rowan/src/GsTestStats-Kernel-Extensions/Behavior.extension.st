Extension { #name : 'Behavior' }

{ #category : '*gsteststats-kernel-extensions' }
Behavior >> includesBehavior: aClass [
	^ self == aClass or: [ self inheritsFrom: aClass ]
]
