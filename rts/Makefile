# the compiler to use
export CC := /usr/local/opt/llvm/bin/clang

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
CFLAGS  = -g -Wall

#files to link:
LDFLAGS += -L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib
#CPPFLAGS += -I/usr/local/opt/llvm/include

# the name to use for both the target source file, and the output file:
TARGET = rts

all: $(TARGET)

$(TARGET): src/$(TARGET).c
	$(CC) $(CFLAGS) -o bin/$(TARGET) src/$(TARGET).c $(CPPFLAGS)

llvm:
	$(CC) -S -emit-llvm -o bin/$(TARGET).ll src/$(TARGET).c

clean:
	\rm -f bin/*; rm -rf bin/rts.dSYM
