#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

using namespace std;

int getMode(int offset, int opcode) {
    int mode = opcode / 100;
    for (int i = 1; i < offset; ++i) {
        mode /= 10;
    }
    mode %= 10;
    return mode;
}

int getParam(int offset, int pointer, vector<string> &memory, int mode) {
    if (mode) {
        return stoi(memory[pointer + offset]);
    }
    return stoi(memory[stoi(memory[pointer + offset])]);
}

string amp(vector<string> memory, const string &setting, const string &input) {
    int pointer = 0;
    bool running = true;
    int a, b, c;
    bool firstInputConsumed = false;
    string output;
    while (running) {
        int opcode = stoi(memory[pointer]);
        switch (opcode % 100) {
            case 1:
                a = getParam(1, pointer, memory, getMode(1, opcode));
                b = getParam(2, pointer, memory, getMode(2, opcode));
                c = getParam(3, pointer, memory, 1);
                memory[c] = to_string(a + b);
                pointer += 4;
                break;
            case 2:
                a = getParam(1, pointer, memory, getMode(1, opcode));
                b = getParam(2, pointer, memory, getMode(2, opcode));
                c = getParam(3, pointer, memory, 1);
                memory[c] = to_string(a * b);
                pointer += 4;
                break;
            case 3:
                a = getParam(1, pointer, memory, 1);
                if (!firstInputConsumed) {
                    memory[a] = setting;
                    firstInputConsumed = true;
                } else {
                    memory[a] = input;

                }
                pointer += 2;
                break;
            case 4:
                a = getParam(1, pointer, memory, 1);
                output = memory[a];
                pointer += 2;
                break;
            case 5:
                a = getParam(1, pointer, memory, getMode(1, opcode));
                b = getParam(2, pointer, memory, getMode(2, opcode));
                if (a != 0) {
                    pointer = b;
                } else {
                    pointer += 3;
                }
                break;
            case 6:
                a = getParam(1, pointer, memory, getMode(1, opcode));
                b = getParam(2, pointer, memory, getMode(2, opcode));
                if (a == 0) {
                    pointer = b;
                } else {
                    pointer += 3;
                }
                break;
            case 7:
                a = getParam(1, pointer, memory, getMode(1, opcode));
                b = getParam(2, pointer, memory, getMode(2, opcode));
                c = getParam(3, pointer, memory, 1);
                memory[c] = (a < b) ? "1" : "0";
                pointer += 4;
                break;
            case 8:
                a = getParam(1, pointer, memory, getMode(1, opcode));
                b = getParam(2, pointer, memory, getMode(2, opcode));
                c = getParam(3, pointer, memory, 1);
                memory[c] = (a == b) ? "1" : "0";
                pointer += 4;
                break;
            case 99:
                running = false;
                break;
            default:
                cout << "Wrong opcode: " << opcode;
                running = false;
                break;
        }
    }
    return output;
}

int main(int argc, char *argv[]) {
    fstream input;
    input.open("day_7.in");
    if (!input.good()) {
        cout << "Can't open file";
        return 1;
    }
    string data;
    getline(input, data);
    istringstream ss(data);
    string token;
    vector<string> memory;
    while (getline(ss, token, ',')) {
        memory.push_back(token);
    }
    int max = 0;
    string answer;
    string config = "01234";
    do {
        string nextInput = "0";
        for (int i = 0; i < 5; ++i) {
            nextInput = amp(memory, config.substr(i, 1), nextInput);
        }
        if (stoi(nextInput) > max) {
            max = stoi(nextInput);
            answer = config;
        }
    } while (next_permutation(config.begin(), config.end()));
    cout << "max output: " << max << "\nconfig: " << config << "\n";

}