SHELL = /bin/sh
CC = g++
INCLUDES = -I/usr/include/vtk-5.8 -I/usr/include/pcl-1.7 -I/usr/include/eigen3 -I/usr/include/ni
CFLAGS       = -fPIC -DEIGEN_USE_NEW_STDVECTOR -DEIGEN_YES_I_KNOW_SPARSE_MODULE_IS_NOT_STABLE_YET -DFLANN_STATIC -Wno-deprecated -shared
LDFLAGS      = -shared

TARGET = libchull.so
HEADERS = ../include/chull.h
SOURCES = convexhull.cpp
OBJECTS = $(SOURCES:.c=.o)

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(FLAGS) $(CFLAGS) $(INCLUDES) -o $(TARGET) $(OBJECTS)

