#pragma once

enum SFileEncoding {
	SFE_UTF8,    // UTF-8
    SFE_UTF8BOM, // UTF-8 with BOM
	SFE_UTF16BE, // UTF-16 big endian
    SFE_UTF16LE, // UTF-16 little endian
    SFE_UTF32BE, // UTF-32 big endian
    SFE_UTF32LE, // UTF-32 little endian
};

enum SFileTypes {
    SFT_UNKNOWN,
    SFT_CUP, // .cup
    SFT_CBC, // .cbc || binary data
    SFT_DLL  // .dll
};

struct SFile {
    const char* name;
    SFileTypes type;
    SFileEncoding encoding;
    size_t size;
    char* data;
};

bool isFileValid(SFile file);
SFileEncoding fileEncoding(const char* data, const size_t size);
SFileTypes extFile(const char* name);
bool readFile(SFile& file, const char* name);