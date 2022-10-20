set(SOURCES
	adepng.h
	adepng.cpp
)

add_library(adepng STATIC ${SOURCES})
target_include_directories(adepng PUBLIC ${CMAKE_CURRENT_LIST_DIR})

