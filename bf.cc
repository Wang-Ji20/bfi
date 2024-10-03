#include <array>
#include <cstdio>
#include <cstdlib>
#include <format>
#include <fstream>
#include <iostream>
#include <stack>

constexpr static int MEMORY_SIZE = 32768;

template <class... Types>
void panic(std::string_view msg, Types &&...args) {
    std::cerr << std::vformat(msg, std::make_format_args(args...));
    exit(EXIT_FAILURE);
}

auto read_file(std::string_view path) -> std::string {
    constexpr auto read_size = std::size_t(4096);
    auto stream = std::ifstream(path.data());
    stream.exceptions(std::ios_base::badbit);

    if (not stream) {
        panic("file not exist.");
    }

    auto out = std::string();
    auto buf = std::string(read_size, '\0');
    while (stream.read(&buf[0], read_size)) {
        out.append(buf, 0, stream.gcount());
    }
    out.append(buf, 0, stream.gcount());
    return out;
}

auto main(int argc, char **argv) -> int {
    std::array<char, MEMORY_SIZE> memory = {0};
    std::stack<int> stack;
    int pointer = 0;

    if (argc != 2) {
        panic("usage: bf <filename>");
    }
    auto source = read_file(argv[1]);

    for (int pc = 0; pc < source.size(); pc++) {
        if (pointer < 0 || pointer >= MEMORY_SIZE) {
            panic("null pointer at {}\n", pointer);
        }
        char c = source[pc];
        switch (c) {
        case '+':
            memory[pointer]++;
            break;
        case '-':
            memory[pointer]--;
            break;
        case '>':
            pointer++;
            break;
        case '<':
            pointer--;
            break;
        case ',':
            memory[pointer] = getchar();
            break;
        case '.':
            putchar(memory[pointer]);
            break;
        case '[':
            if (not memory[pointer]) {
                int nest_counter = 0;
                while (pc < source.size()) {
                    pc++;
                    if (source[pc] == '[') {
                        nest_counter++;
                    }
                    if (source[pc] == ']') {
                        nest_counter--;
                    }
                    if (nest_counter == -1) {
                        break;
                    }
                }
            } else {
                stack.push(pc);
            }
            break;
        case ']':
            if (stack.empty()) {
                panic("pop from empty stack\n");
            }
            if (memory[pointer]) {
                pc = stack.top();
            } else {
                stack.pop();
            }
            break;
        default:
            break;
        }
    }
}
