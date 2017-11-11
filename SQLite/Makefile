current_dir = $(shell pwd)
all:

	ghc --make Test.hs
	cd /run/shm; $(current_dir)/Test

databaseTester:

	ghc --make DatabaseTester.hs
	cd /run/shm; $(current_dir)/DatabaseTester

local:
	ghc --make Test.hs
	./Test

clean:

	rm -f test.db Test.hi Test.o Test testHDBC.db
	cd /run/shm; rm -f test.db testHDBC.db test.db-wal test.db-shm
