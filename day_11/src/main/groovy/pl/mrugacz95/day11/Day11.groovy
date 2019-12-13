package pl.mrugacz95.day11

class IntegerComputer {

    private Map<Long, String> memory
    private Long pointer = 0L
    private Long relativeBase = 0L
    private Boolean running = true
    Closure<Integer> input
    Closure output

    IntegerComputer(Map<Long, String> memory, Closure read, Closure print) {
        this.memory = memory
        this.input = read
        this.output = print
    }

    def readFromMemory(Long position) {
        if (!memory.containsKey(position)) {
            memory[position] = "0"
        }
        return memory[position]
    }

    def getMode(Long offset) {
        def mode = memory[pointer].toLong().intdiv(100)
        (offset - 1).times {
            mode = mode.intdiv(10)
        }
        mode %= 10
        return mode
    }

    def getParam(Long offset) {
        switch (getMode(offset)) {
            case 0: readFromMemory(readFromMemory(pointer + offset).toLong()).toLong() // position
                break
            case 1: readFromMemory((pointer + offset)).toLong() // immediate
                break
            case 2: readFromMemory(
                    relativeBase + readFromMemory((pointer + offset)).toLong()
            ).toLong() // relative
                break
            default: throw new Exception("Unknown position mode: ${memory[pointer]}")
                break
        }
    }

    def saveValue(
            Integer offset,
            Long value
    ) {
        switch (getMode(offset)) {
            case 0: memory[readFromMemory((pointer + offset).toLong()).toLong()] = value.toString()
                break
            case 1: memory[(pointer + offset).toLong()] = value.toString()
                break
            case 2: memory[(relativeBase + readFromMemory((pointer + offset).toLong()).toLong()).toLong()] = value.toString()
                break
        }
    }

    def run() {
        while (running) {
            def opcode = readFromMemory(pointer).toInteger()
            switch (opcode % 100) {
                case 1:
                    def a = getParam(1)
                    def b = getParam(2)
                    saveValue(3, a + b)
                    pointer += 4
                    break
                case 2:
                    def a = getParam(1)
                    def b = getParam(2)
                    saveValue(3, a * b)
                    pointer += 4
                    break
                case 3:
                    Integer input = input.call()
                    saveValue(1, input)
                    pointer += 2
                    break
                case 4:
                    def a = getParam(1)
                    output.call(a)
                    pointer += 2
                    break
                case 5:
                    def a = getParam(1)
                    def b = getParam(2)
                    if (a != 0L) {
                        pointer = b
                    } else {
                        pointer += 3
                    }
                    break
                case 6:
                    def a = getParam(1)
                    def b = getParam(2)
                    if (a == 0L) {
                        pointer = b
                    } else {
                        pointer += 3
                    }
                    break
                case 7:
                    def a = getParam(1)
                    def b = getParam(2)
                    saveValue(3, (a < b) ? 1L : 0L)
                    pointer += 4
                    break
                case 8:
                    def a = getParam(1)
                    def b = getParam(2)
                    saveValue(3, (a == b) ? 1L : 0L)
                    pointer += 4
                    break
                case 9:
                    def a = getParam(1)
                    relativeBase += a
                    pointer += 2
                    break
                case 99:
                    running = false
                    break
                default:
                    println("Wrong opcode: $opcode")
                    running = false
            }
        }
    }
}

class Day11 {
    static void main(String[] args) {
        Map<Integer, String> memory = new File("day_11.in")
                .readLines()[0]
                .split(',')
                .toList()
                .withIndex()
                .collectEntries { code, idx ->
                    [(idx.toLong()): code]
                }
        def ic = new IntegerComputer(memory, {
            return System.in.newReader().readLine().toInteger()
        }, {
            println("output: $it")
        })
        ic.run()
    }
}
