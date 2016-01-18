all: cpu_test

flags.inc: gen_flags.py
	python3 gen_flags.py > flags.inc

asciinums.inc: gen_asciinums.py
	python3 gen_asciinums.py > asciinums.inc

cpu_test.o: cpu_test.s flags.inc print.inc testing.inc asciinums.inc str_testing.inc
	as86 -0 -o cpu_test.o cpu_test.s -l cpu_test.list -s cpu_test.sym

cpu_test: cpu_test.o 
	ld86 -t -d -o cpu_test cpu_test.o

clean:
	rm -f cpu_test.o cpu_test flags.inc

run: cpu_test
	qemu-system-i386 -m 1 -boot a -fda cpu_test

dbg: cpu_test
	qemu-system-i386 -m 1 -fda cpu_test -boot a -monitor stdio -S -s

sdbg:
	qemu-system-i386 -m 1 -fda cpu_test -boot a -monitor stdio -S -s

