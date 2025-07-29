# Makefile for compiling wp13.f95

# Compiler
FC = gfortran

# Compiler flags 
# Moderate optimization
FFLAGS = -O2

# Target executable
TARGET = runwp13

# Source files
SRC = wp13.f95

# Default target
all: $(TARGET)

# Rule to build the target
$(TARGET): $(SRC)
	$(FC) $(FFLAGS) $(SRC) -o $(TARGET)

# Clean up build files
clean:
	rm -f $(TARGET)