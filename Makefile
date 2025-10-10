# Makefile for compiling wp??.f95

# Compiler
FC = gfortran

# Compiler flags 
# Moderate optimization is 2. Higher is 3
FFLAGS = -O2

# Target executable
TARGET = runwp18

# Source files
SRC = wp18.f95

# Default target
all: $(TARGET)

# Rule to build the target
$(TARGET): $(SRC)
	$(FC) $(FFLAGS) $(SRC) -o $(TARGET)

# Clean up build files
clean:
	rm -f $(TARGET)