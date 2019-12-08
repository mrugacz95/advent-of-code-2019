#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

#define AMPS_NUM 5
using namespace std;

class AMP {
private:
    vector<string> memory;
    bool halted;
    int pointer = 0;
    int id;
public:
    explicit AMP(const vector<string> &memory, const string &setting, int id) {
        this->memory = memory;
        this->halted = false;
        this->run(setting, true);
        this->id = id;
    }

    static int getMode(int offset, int opcode) {
        int mode = opcode / 100;
        for (int i = 1; i < offset; ++i) {
            mode /= 10;
        }
        mode %= 10;
        return mode;
    }

    static int getParam(int offset, int pointer, vector<string> &memory, int mode) {
        if (mode) {
            return stoi(memory[pointer + offset]);
        }
        return stoi(memory[stoi(memory[pointer + offset])]);
    }

    string run(const string &input, bool waitForNextInput) {
        bool running = true;
        int a, b, c;
        string output;
        bool firstInputConsumed = false;
        while (!halted) {
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
                    if (firstInputConsumed) {
                        return {}; // wait for next input, resume again from this instruction
                    }
                    firstInputConsumed = true;
                    a = getParam(1, pointer, memory, 1);
                    memory[a] = input;
                    pointer += 2;
                    break;
                case 4:
                    a = getParam(1, pointer, memory, 1);
                    output = memory[a];
                    pointer += 2;
                    return output; // finish running
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
                    halted = true;
                    break;
                default:
                    cout << "Wrong opcode: " << opcode;
                    halted = true;
                    break;
            }
        }
        return {};
    }

    bool isHalted() {
        return halted;
    }
};

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
        memory.emplace_back(token);
    }
    int max = 0;
    string answer;
    string config = "56789";
    do {
        vector<AMP> amps;
        amps.reserve(AMPS_NUM);
        for (int i = 0; i < AMPS_NUM; ++i) {
            amps.emplace_back(memory, config.substr(i, 1), i);
        }
        string nextInput = "0";
        int i = 0;
        string lastEOutput;
        while (!amps[amps.size() - 1].isHalted()) {
            nextInput = amps[(i++) % AMPS_NUM].run(nextInput, false);
            if ((i++) % AMPS_NUM == 4) {
                lastEOutput = nextInput;
            }
        }
        if (stoi(lastEOutput) > max) {
            max = stoi(lastEOutput);
            answer = config;
        }
    } while (next_permutation(config.begin(), config.end()));
    cout << "max output: " << max << "\nconfig: " << answer << "\n";

}