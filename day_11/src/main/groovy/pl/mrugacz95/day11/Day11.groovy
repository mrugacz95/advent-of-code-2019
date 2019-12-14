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


class Robot {
    def position = new Tuple2<Integer, Integer>(0, 0)
    enum Direction {
        UP, DOWN, LEFT, RIGHT
    }
    def direction = Direction.UP
    enum Color {
        BLACK, WHITE, NOT_PAINTED
    }

    enum Rotate {
        LEFT, RIGHT
    }
    HashMap<Tuple2<Integer, Integer>, Color> board = new HashMap<>()

    def moveForward() {
        def x = 0
        def y = 0
        switch (direction) {
            case Direction.UP:
                y -= 1
                break
            case Direction.DOWN:
                y += 1
                break
            case Direction.LEFT:
                x -= 1
                break
            case Direction.RIGHT:
                x += 1
                break
        }
        position = new Tuple2<Integer, Integer>(position.first + x, position.second + y)
    }

    def paint(Color color) {
        board[position] = color
    }

    def rotate(Rotate rotate) {
        if (rotate == Rotate.LEFT) {
            switch (direction) {
                case Direction.UP:
                    direction = Direction.LEFT
                    break
                case Direction.DOWN:
                    direction = Direction.RIGHT
                    break
                case Direction.LEFT:
                    direction = Direction.DOWN
                    break
                case Direction.RIGHT:
                    direction = Direction.UP
                    break
            }
        } else {
            switch (direction) {
                case Direction.UP:
                    direction = Direction.RIGHT
                    break
                case Direction.DOWN:
                    direction = Direction.LEFT
                    break
                case Direction.LEFT:
                    direction = Direction.UP
                    break
                case Direction.RIGHT:
                    direction = Direction.DOWN
                    break
            }
        }
    }

    def readColor() {
        return readColor(position)
    }

    def readColor(Tuple2<Integer, Integer> pos) {
        if (!board.containsKey(pos)) {
            return Color.NOT_PAINTED
        }
        return board[pos]
    }

    def countPainted() {
        return board.size()
    }

    def printBoard() {
        def maxX = -Integer.MAX_VALUE
        def minX = Integer.MAX_VALUE
        def maxY = -Integer.MAX_VALUE
        def minY = Integer.MAX_VALUE
        board.forEach { tuple, color ->
            maxX = Math.max(tuple.first, maxX)
            minX = Math.min(tuple.first, minX)
            maxY = Math.max(tuple.second, maxY)
            minY = Math.min(tuple.second, minY)
        }
        minY.upto(maxY, { y ->
            minX.upto(maxX, { x ->
                def pos = new Tuple2(x, y)
                if (pos == position) {
                    switch (direction) {
                        case Direction.UP:
                            print("^")
                            break
                        case Direction.DOWN:
                            print("V")
                            break
                        case Direction.LEFT:
                            print("<")
                            break
                        case Direction.RIGHT:
                            print(">")
                            break
                    }
                } else {
                    switch (readColor(pos)) {
                        case Color.BLACK:
                            print('.')
                            break
                        case Color.WHITE:
                            print('#')
                            break
                        case Color.NOT_PAINTED:
                            print(' ')
                            break
                    }
                }
            })
            println()
        })
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
        def robot = new Robot()
        def colorConsumed = false
        def ic = new IntegerComputer(memory, {
            return robot.readColor() != Robot.Color.WHITE ? 0 : 1
        }, {
            if (!colorConsumed) {
                robot.paint(it == 0 ? Robot.Color.BLACK : Robot.Color.WHITE)
                colorConsumed = true
            } else {
                robot.rotate(it == 0 ? Robot.Rotate.LEFT : Robot.Rotate.RIGHT)
                robot.moveForward()
                colorConsumed = false
            }
        })
        ic.run()
        robot.printBoard()
        println(robot.countPainted())
    }
}
