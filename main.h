#pragma once
#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stack>
#include <map>
#include <Windows.h>

#define CUPSUCCESS false
#define CUPERROR true

void ptrError(const char* line, const char* index, const char* err);
bool strpcmp(const char* a, const char* b, const char* b_end);

template<typename T>
struct linked_list {
	T* begin;
	T* end;
};