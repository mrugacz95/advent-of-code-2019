local open = io.open
IntegerComputer = require "integer_computer"

local path = "day_17.in"
local file = open(path, "r")
local content = file:read "*a"
file:close()

local memory = {}
for i in string.gmatch(content, "-?%d+") do
   table.insert(memory, tonumber(i))
end
function printValue(value)
    print("output: ", value)
end
function readValue()
    return io.read("*n")
end


local ic = IntegerComputer:new(memory, readValue, printValue)
ic:run()