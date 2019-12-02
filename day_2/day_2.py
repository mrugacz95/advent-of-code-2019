ALARM = True


def main():
    pointer = 0
    memory = list(map(int, open('day_2.in').read().split(',')))

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

    if ALARM:
        memory[1] = 12
        memory[2] = 2

    while running:
        opcode = memory[pointer]
        pointer = {
            1: add,
            2: mul,
            99: stop
        }.get(opcode)(pointer)
    print(memory[0])


if __name__ == '__main__':
    main()
