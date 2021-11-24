Extension { #name : 'TestCase' }

{ #category : '*gsteststats-kernel-extensions' }
TestCase >> expectedFailures [
	^ Array new
]

{ #category : '*gsteststats-kernel-extensions' }
TestCase >> shouldPass [
	"Unless the selector is in the list we get from #expectedFailures, we expect it to pass"

	^ (self expectedFailures includes: testSelector) not
]
