#ifndef TSG_UTIL
#define TSG_UTIL 1

#include <fstream>
#include <iostream>

namespace {

    void readLEbytes(std::ifstream& ifs, char* data, size_t bytes) {
        for(int i = bytes - 1;i>=0;--i) {
            ifs.read(data + i,1);
        }
    }
    
    void writeBEbytes(std::ofstream& ofs, char* data, size_t bytes) {
        for(int i = bytes - 1;i>=0;--i) {
            ofs.write(data + i,1);
        }
    }
    
};

#endif
