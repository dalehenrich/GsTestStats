Extension { #name : 'CharacterCollection' }

{ #category : '*gsteststats-kernel-extensions' }
CharacterCollection >> contractTo: smallSize [
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
]

{ #category : '*gsteststats-kernel-extensions' }
CharacterCollection >> truncateTo: smallSize [
	"return myself or a copy shortened to smallSize.  1/18/96 sw"

	^ self size <= smallSize
		ifTrue: [ self ]
		ifFalse: [ self copyFrom: 1 to: smallSize ]
]
