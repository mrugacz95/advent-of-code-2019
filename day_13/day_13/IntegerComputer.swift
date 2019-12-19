class IntegerComputer {
    var memory: [Int:Int]
    var input: () -> (Int)
    var output: (Int) -> ()
    var pointer : Int = 0
    var relativeBase: Int = 0
    var running = false
    
    init(memory: [Int:Int],  input: @escaping () -> Int, output: @escaping (Int) -> Void ) {
        self.memory = memory
        self.input = input
        self.output = output
    }
    
    func getMode(_ offset: Int) -> Int {
        var mode = readFromMemory(self.pointer) / 100
        for _ in 1..<offset {
            mode = mode / 10
        }
        mode %= 10
        return mode
    }
    
    func getParam(_ offset: Int) -> Int {
        let mode = getMode(offset)
        switch (mode) {
        case 0:
            return readFromMemory(readFromMemory(pointer + offset)) // position
        case 1:
            return readFromMemory((pointer + offset)) // immediate
        case 2:
            return readFromMemory( relativeBase + readFromMemory((pointer + offset))) // relative
        default:
            print("Unknown position mode: /(mode)")
            running = false
            return 0
        }
    }
    
    func readFromMemory(_ position: Int) -> Int {
        if let value = memory[position] {
            return value
        } else {
            memory[position] = 0
            return 0
        }
    }
    
    func saveValue(offset: Int, value : Int) {
        let mode = getMode(offset)
        switch mode {
        case 0: memory[readFromMemory(pointer + offset)] = value
            break
        case 1: memory[(pointer + offset)] = value
            break
        case 2: memory[(relativeBase + readFromMemory((pointer + offset)))] = value
            break
        default:
            print("Unsupported save more \(mode)")
            running = false
        }
    }
    func run() {
        running = true
        while (running) {
            let opcode = readFromMemory(pointer)
            switch (opcode % 100) {
            case 1:
                let a = getParam(1)
                let b = getParam(2)
                saveValue(offset: 3, value: a + b)
                pointer += 4
                break
            case 2:
                let a = getParam(1)
                let b = getParam(2)
                saveValue(offset: 3, value: a * b)
                pointer += 4
                break
            case 3:
                saveValue(offset:1, value:input())
                pointer += 2
                break
            case 4:
                let a = getParam(1)
                output(a)
                pointer += 2
                break
            case 5:
                let a = getParam(1)
                let b = getParam(2)
                if (a != 0) {
                    pointer = b
                } else {
                    pointer += 3
                }
                break
            case 6:
                let a = getParam(1)
                let b = getParam(2)
                if (a == 0) {
                    pointer = b
                } else {
                    pointer += 3
                }
                break
            case 7:
                let a = getParam(1)
                let b = getParam(2)
                saveValue(offset: 3, value: (a < b) ? 1 : 0)
                pointer += 4
                break
            case 8:
                let a = getParam(1)
                let b = getParam(2)
                saveValue(offset: 3, value: (a == b) ? 1 : 0)
                pointer += 4
                break
            case 9:
                let a = getParam(1)
                relativeBase += a
                pointer += 2
                break
            case 99:
                running = false
                break
            default:
                print("Wrong opcode: $opcode")
                running = false
            }
        }
    }
}
