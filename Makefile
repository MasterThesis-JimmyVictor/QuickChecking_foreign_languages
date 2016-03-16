current_dir = $(shell pwd)
all:

	ghc Test.hs
	cd /run/shm; $(current_dir)/Test

clean:

	rm -f test.db Test.hi Test.o Test
	cd /run/shm; rm -f test.db
