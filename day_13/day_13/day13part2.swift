import Foundation

func loadFile(filepath: String) -> String{
    if let filepath = Bundle.main.path(forResource: file, ofType: nil) {
        do {
            return try String(contentsOfFile: filepath)
        } catch {
            print("File couldn't be open")
            exit(0)
        }
    }
    else {
        print("File not found")
        exit(0)
    }
}

let file = "day_13.in"
var input: String = loadFile(filepath: file)

var memory = Dictionary(uniqueKeysWithValues:
    input.trimmingCharacters(in: .whitespacesAndNewlines)
        .split(separator: ",")
        .map{Int(String($0))!}
        .enumerated()
        .map{ ($0, $1) }
)
memory[0] = 2
struct Coord: Hashable {
    let x: Int
    let y: Int
    
    init(x:Int, y: Int){
        self.x = x
        self.y = y
    }
}
enum Type : Int {
    case empty, wall, block, paddle, ball
}
var screen = [Coord:Type]()


var x = 0
var y = 0
var inputType = 0
var score = 0

func display(){
    var maxX = 0
    var minX = 0
    var maxY = 0
    var minY = 0
    for (coord, _) in screen {
        maxX = max(maxX, coord.x)
        minX = min(minX, coord.x)
        maxY = max(maxY, coord.y)
        minY = min(minY, coord.y)
    }
    print("\u{001B}[2J")
    for y in minY...maxY {
        for x in minX...maxX {
            if let block = screen[Coord(x:x, y:y)] {
                switch(block){
                case Type.empty: print(" ", terminator: " ")
                    break
                case Type.block: print("⊞", terminator: " ")
                    break
                case Type.wall: print("█", terminator: " ")
                    break
                case Type.paddle: print("☰", terminator: " ")
                    break
                case Type.ball: print("○", terminator: " ")
                    break
                }
            } else {
                print(" ", terminator: " ")
            }
        }
        print()
    }
    print("Score \(score)")
    usleep(55000)
}
var lastBallPos: Coord?  = nil
func makeAction() -> Int {
    func find(type: Type) -> Coord? {
        for (coord, block) in screen {
            if block == type {
                return coord
            }
        }
        return nil
    }
    let ballPos = find(type: Type.ball)! // Ball for sure is somewhere
    let paddlePos = find(type: Type.paddle)!
    lastBallPos = ballPos
    if paddlePos.x > ballPos.x {
        return -1
    } else if paddlePos.x == ballPos.x {
        return 0
    } else {
        return 1
    }
}

let ic = IntegerComputer(memory: memory, input: {
//    display()
    return makeAction()
}) { (number:Int) -> ()  in
    switch(inputType){
    case 0: x = number
        break
    case 1: y = number
        break
    default:
        if(x == -1 && y == 0){
            score = number
        } else {
            screen[Coord(x: x, y: y)] = Type.init(rawValue: number)
        }
    }
    inputType = (inputType + 1) % 3
}

ic.run()

print(score)
