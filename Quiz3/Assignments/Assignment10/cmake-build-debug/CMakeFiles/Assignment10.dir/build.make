# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.16

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /mnt/d/Projects/Fortran/Assignment10

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /mnt/d/Projects/Fortran/Assignment10/cmake-build-debug

# Include any dependencies generated for this target.
include CMakeFiles/Assignment10.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/Assignment10.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/Assignment10.dir/flags.make

CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.o: CMakeFiles/Assignment10.dir/flags.make
CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.o: ../MohammadrezaKazemi_Assignment08.f95
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/mnt/d/Projects/Fortran/Assignment10/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.o"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /mnt/d/Projects/Fortran/Assignment10/MohammadrezaKazemi_Assignment08.f95 -o CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.o

CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.i"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /mnt/d/Projects/Fortran/Assignment10/MohammadrezaKazemi_Assignment08.f95 > CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.i

CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.s"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /mnt/d/Projects/Fortran/Assignment10/MohammadrezaKazemi_Assignment08.f95 -o CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.s

# Object files for target Assignment10
Assignment10_OBJECTS = \
"CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.o"

# External object files for target Assignment10
Assignment10_EXTERNAL_OBJECTS =

Assignment10: CMakeFiles/Assignment10.dir/MohammadrezaKazemi_Assignment08.f95.o
Assignment10: CMakeFiles/Assignment10.dir/build.make
Assignment10: CMakeFiles/Assignment10.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/mnt/d/Projects/Fortran/Assignment10/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable Assignment10"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/Assignment10.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/Assignment10.dir/build: Assignment10

.PHONY : CMakeFiles/Assignment10.dir/build

CMakeFiles/Assignment10.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/Assignment10.dir/cmake_clean.cmake
.PHONY : CMakeFiles/Assignment10.dir/clean

CMakeFiles/Assignment10.dir/depend:
	cd /mnt/d/Projects/Fortran/Assignment10/cmake-build-debug && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/d/Projects/Fortran/Assignment10 /mnt/d/Projects/Fortran/Assignment10 /mnt/d/Projects/Fortran/Assignment10/cmake-build-debug /mnt/d/Projects/Fortran/Assignment10/cmake-build-debug /mnt/d/Projects/Fortran/Assignment10/cmake-build-debug/CMakeFiles/Assignment10.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/Assignment10.dir/depend
