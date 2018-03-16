
#!/bin/sh
./buildsmugc.sh && ./smugc.sh tests/numericliteraltests.sm
if [ $? -eq 0 ]
	then
		echo "C program execution:"
		./c.out
		echo "LLVM program execution:"
		./llvm.out
fi