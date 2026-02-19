bin/InCollegeDriver: src/InCollegeDriver.cob bin/AccountLogic.o bin/ProfileLogic.o
	cobc -x -free -o bin/InCollegeDriver src/InCollegeDriver.cob bin/AccountLogic.o bin/ProfileLogic.o

bin/AccountLogic.o: src/AccountLogic.cob
	cobc -c -free -o bin/AccountLogic.o src/AccountLogic.cob

bin/ProfileLogic.o: src/ProfileLogic.cob
	cobc -c -free -o bin/ProfileLogic.o src/ProfileLogic.cob

clean:
	rm -f bin/InCollegeDriver bin/AccountLogic.o bin/ProfileLogic.o