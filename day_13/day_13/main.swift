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

let ic = IntegerComputer(memory: memory, input: {
   return Int(readLine(strippingNewline: true)!)!
}) { (number:Int) -> ()  in
    print(number)
}

ic.run()
