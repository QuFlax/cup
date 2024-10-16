#include "main.h"
#include "files.h"

bool isFileValid(SFile file) {
    return file.data != nullptr;
}

SFileEncoding fileEncoding(const char* data, const size_t size) {
    if (size > 2) {
        // FE FF UTF-16, big-endian
        if (data[0] == 0xFE && data[1] == 0xFF) {
            return SFE_UTF16BE;
        }
        // FF FE UTF-16, little-endian
        if (data[0] == 0xFF && data[1] == 0xFE) {
            return SFE_UTF16LE;
        }
        if (size > 3) {
            // EF BB BF UTF-8
            if (data[0] == 0xEF && data[1] == 0xBB && data[2] == 0xBF) {
                return SFE_UTF8BOM;
            }
            if (size > 4) {
                // 00 00 FE FF UTF-32, big-endian
                if (data[0] == 0 && data[1] == 0 && data[2] == 0xFE && data[3] == 0xFF) {
                    return SFE_UTF32BE;
                }
                // FF FE 00 00 UTF-32, little-endian
                if (data[0] == 0xFF && data[1] == 0xFE && data[2] == 0 && data[3] == 0) {
                    return SFE_UTF32LE;
                }
            }
        }
    }
    return SFE_UTF8;
}

SFileTypes extFile(const char* name) {
    // TODO: MAYBE READ FIRST 4 BYTES FOR TYPE
    const char* ext = strrchr(name, '.');
    if (ext == nullptr)
        return SFT_CBC;
    if (!strcmp(ext, ".cbc"))
        return SFT_CBC;
    if (!strcmp(ext, ".cup"))
        return SFT_CUP;
    if (!strcmp(ext, ".dll"))
        return SFT_DLL;
    return SFT_UNKNOWN;
}

bool readFile(SFile& file, const char* name) {
    FILE* fp = fopen(name, "rb");
    if (!fp)
        return CUPERROR;

    file.name = name;
    file.type = extFile(name);

    fseek(fp, 0, SEEK_END);
    file.size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    file.data = new char[file.size + 1];
	file.data[file.size] = '\0';
    fread(file.data, file.size, 1, fp);
    fclose(fp);

    file.encoding = fileEncoding(file.data, file.size);

    return CUPSUCCESS;
}