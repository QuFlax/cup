#include "cup.h"

static CUPState state = {};

CVariable extental(const char* str) {
	printf("extental: %s\n", str);
	if (strcmp(str, "putchar") == 0) {
		return {
			{ CType::getFunction(CType::getVoid(), { CType::getNumber() }), (size_t)putchar },
			CVariableFlag::SVF_GLOBAL | CVariableFlag::SVF_CONSTANT
		};
	}
	if (strcmp(str, "puts") == 0) {
		return {
			{ CType::getFunction(CType::getVoid(), { CType::getNumber() }), (size_t)puts },
			CVariableFlag::SVF_GLOBAL | CVariableFlag::SVF_CONSTANT
		};
	}
	char buf[256];
	sprintf_s(buf, sizeof(buf), "extental: function '%s' not found", str);
	state.error(buf);
	exit(1);
}

int main() {
	state.input_path = "Z:\\main - Copy (2).cup";
	FILE* f;
	fopen_s(&f, state.input_path, "rb");
	fseek(f, 0, SEEK_END);
	size_t size = ftell(f);
	fseek(f, 0, SEEK_SET);
	state.input_stream = new char[size + 1];
	char* ptr = (char*)state.input_stream;
	ptr[size] = '\0';
	fread(ptr, size, 1, f);
	fclose(f);

	// state.input_path = "Z:\\Projects\\CUPTEST\\Source.cup";
	// state.input_stream = "a = b\nc = a + b";

	set_error_function(&state, [](const char* msg) {
		fputs(msg, stderr);
		});
	set_machine_type(&state, CUPMachineType::CIRM_X64);
	parse(&state);

	set_extental_function(&state, extental);
  compile(&state);
	return 0;
}
