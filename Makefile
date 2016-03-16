all:

	ghc Test.hs
	./Test


clean:

	rm -f test.db Test.hi Test.o Test
