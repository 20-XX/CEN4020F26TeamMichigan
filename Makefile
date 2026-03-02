bin/InCollegeDriver: src/InCollegeDriver.cob bin/AccountLogic.o bin/ProfileLogic.o bin/ConnectionLogic.o
	cobc -x -free -o bin/InCollegeDriver src/InCollegeDriver.cob bin/AccountLogic.o bin/ProfileLogic.o bin/ConnectionLogic.o

bin/AccountLogic.o: src/AccountLogic.cob
	cobc -c -free -o bin/AccountLogic.o src/AccountLogic.cob

bin/ProfileLogic.o: src/ProfileLogic.cob
	cobc -c -free -o bin/ProfileLogic.o src/ProfileLogic.cob

bin/ConnectionLogic.o: src/ConnectionLogic.cob
	cobc -c -free -o bin/ConnectionLogic.o src/ConnectionLogic.cob

clean:
	rm -f bin/InCollegeDriver bin/AccountLogic.o bin/ProfileLogic.o bin/ConnectionLogic.o