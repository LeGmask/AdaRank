make:
	mkdir -p build
	cd build && gnatmake -gnatwa -gnata -g -d ../src/page_rank.adb

make_profiler:
	mkdir -p build
	cd build && gnatmake -gnatwa -gnata -g -d -pg ../src/page_rank.adb

profile:
	gprof build/page_rank | gprof2dot -n 0 -e 0 | dot -Tpng -o build/profile.png
	xdg-open build/profile.png

clean:
	cd build/ && gnatclean ../src/*.adb && gnatclean ../tests/*.adb && rm -f *.png
	rm -f *.prw *.pr
	rm -f gmon.out
	rm -f benchmark.md

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
