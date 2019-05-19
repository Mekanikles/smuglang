
#!/bin/sh
./buildsmugc.sh && ./smugc.sh tests/templates.sm
if [ $? -eq 0 ]
	then
		echo "LLVM program execution:"
		./llvm.out
fi