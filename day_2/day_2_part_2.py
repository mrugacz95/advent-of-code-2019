ALARM = True


def load_mem():
    return list(map(int, open('day_2.in').read().split(',')))


def run_computer(noun, verb):
    pointer = 0
    memory = load_mem()

    def add(pointer):
        a = memory[memory[pointer + 1]]
        b = memory[memory[pointer + 2]]
        memory[memory[pointer + 3]] = a + b
        return pointer + 4

    def mul(pointer):
        a = memory[memory[pointer + 1]]
        b = memory[memory[pointer + 2]]
        memory[memory[pointer + 3]] = a * b
        return pointer + 4

    running = True

    def stop(pointer):
        nonlocal running
        running = False
        return 0

    memory[1] = noun
    memory[2] = verb

    while running:
        opcode = memory[pointer]
        pointer = {
            1: add,
            2: mul,
            99: stop
        }.get(opcode)(pointer)
    return memory[0]


def main():
    for i in range(100):
        for j in range(100):
            if run_computer(i, j) == 19690720:
                print(i * 100 + j)


if __name__ == '__main__':
    main()
