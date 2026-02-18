bin/InCollegeDriver: src/InCollegeDriver.cob bin/AcctMgr.o bin/ProfMgr.o
	cobc -x -free -o bin/InCollegeDriver src/InCollegeDriver.cob bin/AcctMgr.o bin/ProfMgr.o

bin/AcctMgr.o: src/AcctMgr.cob
	cobc -c -free -o bin/AcctMgr.o src/AcctMgr.cob

bin/ProfMgr.o: src/ProfMgr.cob
	cobc -c -free -o bin/ProfMgr.o src/ProfMgr.cob

clean:
	rm -f bin/InCollegeDriver bin/AcctMgr.o bin/ProfMgr.o