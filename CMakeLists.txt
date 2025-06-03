cmake_minimum_required(VERSION 3.16)
project(cup)
set(CMAKE_CXX_STANDARD 23)

find_package(ZLIB REQUIRED)
find_package(LLVM REQUIRED CONFIG COMPONENTS Core ExecutionEngine OrcJIT)
find_package(LLD CONFIG REQUIRED)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Found LLVM_INCLUDE_DIRS ${LLVM_INCLUDE_DIRS}")
message(STATUS "Found LLD_INCLUDE_DIRS ${LLD_INCLUDE_DIRS}")

# Map LLVM components to libraries
llvm_map_components_to_libnames(llvm_libs
    Core
    ExecutionEngine
    OrcJIT
    OrcDebugging
    OrcShared
    OrcTargetProcess
    Support
)

add_executable(cup main.cpp)

# Set include directories
target_include_directories(cup PRIVATE 
    ${LLVM_INCLUDE_DIRS}
    ${LLD_INCLUDE_DIRS}
)
target_compile_definitions(cup PRIVATE ${LLVM_DEFINITIONS})

# Use the correct LLD libraries from LLD_EXPORTED_TARGETS
target_link_libraries(cup PRIVATE 
    ${llvm_libs}
    ZLIB::ZLIB
    lldCommon
    lldCOFF
	lldELF
	lldMachO
	lldMinGW
	lldWasm
)

# Add LLVM's library directory to linker search path
target_link_directories(cup PRIVATE ${LLVM_LIBRARY_DIRS})