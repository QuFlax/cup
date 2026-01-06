CFLAGS = -ggdb -Wall -Wextra -O0 -I./
LDFLAGS = -lm
LIB_NAME = libcup
CUP_NAME = cup.elf

all: $(LIB_NAME).so $(LIB_NAME).a $(CUP_NAME)

$(LIB_NAME).a: $(LIB_NAME).o
		ar rcs $@ $^

$(LIB_NAME).so: $(LIB_NAME).o
		$(CXX) -shared -o $@ $^ $(LDFLAGS)

$(LIB_NAME).o: ./libcup.cpp
		$(CXX) $(CFLAGS) -fPIC -c $< -o $@

$(CUP_NAME): ./main.cpp $(LIB_NAME).so $(LIB_NAME).a
		$(CXX) $(CFLAGS) ./main.cpp -L. -lcup $(LDFLAGS) -Wl,-rpath,'$$ORIGIN' -o $@

# Run test (now works without LD_LIBRARY_PATH)
run: $(CUP_NAME)
		./$(CUP_NAME)

debug: $(CUP_NAME)
		lldb ./$(CUP_NAME)

ldd: $(CUP_NAME)
		ldd ./$(CUP_NAME)

clean:
		rm -f ./*.o $(LIB_NAME).so $(LIB_NAME).a $(CUP_NAME)

.PHONY: all run debug ldd clean
