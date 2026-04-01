bin/InCollege: src/InCollege.cob build/AccountLogic.o build/ProfileLogic.o build/ConnectionLogic.o build/JobLogic.o build/MessageLogic.o
	cobc -x -free -o bin/InCollege src/InCollege.cob build/AccountLogic.o build/ProfileLogic.o build/ConnectionLogic.o build/JobLogic.o build/MessageLogic.o

build/AccountLogic.o: src/AccountLogic.cob
	cobc -c -free -o build/AccountLogic.o src/AccountLogic.cob

build/ProfileLogic.o: src/ProfileLogic.cob
	cobc -c -free -o build/ProfileLogic.o src/ProfileLogic.cob

build/ConnectionLogic.o: src/ConnectionLogic.cob
	cobc -c -free -o build/ConnectionLogic.o src/ConnectionLogic.cob

build/JobLogic.o: src/JobLogic.cob
	cobc -c -free -o build/JobLogic.o src/JobLogic.cob

build/MessageLogic.o: src/MessageLogic.cob
	cobc -c -free -o build/MessageLogic.o src/MessageLogic.cob

clean:
	rm -f bin/InCollege build/AccountLogic.o build/ProfileLogic.o build/ConnectionLogic.o build/JobLogic.o build/MessageLogic.o