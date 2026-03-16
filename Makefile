bin/InCollege: src/InCollege.cob build/AccountLogic.o build/ProfileLogic.o build/ConnectionLogic.o
	cobc -x -free -o bin/InCollege src/InCollege.cob build/AccountLogic.o build/ProfileLogic.o build/ConnectionLogic.o

build/AccountLogic.o: src/AccountLogic.cob
	cobc -c -free -o build/AccountLogic.o src/AccountLogic.cob

build/ProfileLogic.o: src/ProfileLogic.cob
	cobc -c -free -o build/ProfileLogic.o src/ProfileLogic.cob

build/ConnectionLogic.o: src/ConnectionLogic.cob
	cobc -c -free -o build/ConnectionLogic.o src/ConnectionLogic.cob

clean:
	rm -f bin/InCollege build/AccountLogic.o build/ProfileLogic.o build/ConnectionLogic.o