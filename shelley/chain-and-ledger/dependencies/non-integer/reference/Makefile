OBJ = non_integral.o
CFLAGS = -O2
OFLAGS = -fPIC
LFLAGS = -lgmp -lgmpxx

all: non_integral_test libnon_integral

non_integral_test: non_integral.o non_integral.hpp non_integral_test.cpp
	$(CXX) $(CFLAGS) -o non_integral_test non_integral_test.cpp $(OBJ) $(LFLAGS)

non_integral.o: non_integral.c non_integral.h
	$(CC) $(CFLAGS) $(OFLAGS) -c non_integral.c

libnon_integral: non_integral.o
	$(CC) -shared -o libnon_integral.so non_integral.o

.PHONY: clean
clean:
	rm non_integral_test libnon_integral.so $(OBJ)
