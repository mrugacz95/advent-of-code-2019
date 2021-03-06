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

let ic = IntegerComputer(memory: memory, input: {
    return Int(readLine(strippingNewline: true)!)!
}) { (number:Int) -> ()  in
    switch(inputType){
    case 0: x = number
        break
    case 1: y = number
        break
    default:
        screen[Coord(x: x, y: y)] = Type.init(rawValue: number)
    }
    inputType = (inputType + 1) % 3
}

ic.run()

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
    for x in minX...maxX {
        for y in minY...maxY {
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
    }
}

var blockTiles = 0
for (_, block) in screen {
    if block == Type.block {
        blockTiles += 1
    }
}
print(blockTiles)
