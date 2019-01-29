
#!/bin/sh
./buildsmugc.sh && ./smugc.sh tests/tests.sm
if [ $? -eq 0 ]
	then
		echo "LLVM program execution:"
		./llvm.out
fi