make:
	mkdir -p build
	cd build && gnatmake -gnatwa -gnata -g -d ../src/page_rank.adb

clean:
	cd build/ && gnatclean ../src/*.adb && gnatclean ../tests/*.adb
	rm -f *.prw *.pr
test:
	mkdir -p build
	cd build/ && gnatmake -gnatwa -gnata -d -aI../src/ -g ../tests/tests.adb

# pass arguments through ARGS="arg1 arg2" make run
run:
	./build/page_rank $(ARGS)

run_test:
	./build/tests

%:
	@: