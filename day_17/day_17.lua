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
local cameraView = {}
local cameraViewRow = {}
function printValue(value)
    local char = string.char(value)
    if char == "\n" then
        table.insert(cameraView, cameraViewRow)
        cameraViewRow = {}
    else
        table.insert(cameraViewRow, char)
    end
end
function readValue()
    return io.read("*n")
end

local ic = IntegerComputer:new(memory, readValue, printValue)
ic:run()

function printCameraView()
    for _, row in pairs(cameraView)
    do
        for _, cell in pairs(row)
        do
            io.write(cell)
        end
        print()
    end
end

--[[
-- test data
cameraView = { { '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', },
             { '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', },
             { '#', '#', '#', '#', '#', '#', '#', '.', '.', '.', '#', '#', '#', },
             { '#', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#', '.', '#', },
             { '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', },
             { '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.', },
             { '.', '.', '#', '#', '#', '#', '#', '.', '.', '.', '^', '.', '.' } }
]]--
printCameraView()

function countIntersections()
    local sum = 0
    for y, row in pairs(cameraView)
    do
        for x, _ in pairs(row)
        do
            if x > 1 and y > 1 and y < #cameraView and x < #row and cameraView[y][x] == '#' then

                local intersection = true
                local dim = { { 1, 0 }, { 0, 1 }, { -1, 0 }, { 0, -1 } }
                for _, d in pairs(dim)
                do
                    if cameraView[y + d[1]][x + d[2]] ~= '#' then
                        intersection = false
                        break
                    end
                end
                if intersection == true then
                    sum = sum + (x - 1) * (y - 1)
                end
            end
        end
    end
    return sum
end

print(countIntersections())