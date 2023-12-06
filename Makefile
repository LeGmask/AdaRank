make:
	mkdir -p build
	cd build && gnatmake -gnatwa -gnata -g -d ../src/page_rank.adb

clean:
	cd build/ && gnatclean ../src/*.adb && gnatclean ../tests/*.adb

test:
	mkdir -p build
	cd build/ && gnatmake -gnatwa -gnata  -aI../src/ -g ../tests/tests.adb

run:
	./build/page_rank

run_test:
	./build/tests

