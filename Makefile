bin/InCollege: src/InCollege.cob build/AccountLogic.o build/ProfileLogic.o build/ConnectionLogic.o build/JobLogic.o build/MessageLogic.o | bin
	cobc -x -free -o bin/InCollege src/InCollege.cob build/AccountLogic.o build/ProfileLogic.o build/ConnectionLogic.o build/JobLogic.o build/MessageLogic.o

build/AccountLogic.o: src/AccountLogic.cob | build
	cobc -c -free -o build/AccountLogic.o src/AccountLogic.cob

build/ProfileLogic.o: src/ProfileLogic.cob | build
	cobc -c -free -o build/ProfileLogic.o src/ProfileLogic.cob

build/ConnectionLogic.o: src/ConnectionLogic.cob | build
	cobc -c -free -o build/ConnectionLogic.o src/ConnectionLogic.cob

build/JobLogic.o: src/JobLogic.cob | build
	cobc -c -free -o build/JobLogic.o src/JobLogic.cob

build/MessageLogic.o: src/MessageLogic.cob | build
	cobc -c -free -o build/MessageLogic.o src/MessageLogic.cob

build:
	mkdir -p build

bin:
	mkdir -p bin

clean:
	rm -f bin/InCollege build/*.o