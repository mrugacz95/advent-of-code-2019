local open = io.open
IntegerComputer = require "integer_computer"
function readMemory()
    local path = "day_17.in"
    local file = open(path, "r")
    local content = file:read "*a"
    file:close()

    local mem = {}
    for i in string.gmatch(content, "-?%d+") do
        table.insert(mem, tonumber(i))
    end
    return mem
end
local memory = readMemory()
local cameraView = {}
local cameraViewRow = {}
function saveCameraStatus(value)
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

-- read map
local ic = IntegerComputer:new(memory, readValue, saveCameraStatus)
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

--cameraView = { { '#', '#', '#', '#', '#', '#', '#', '.', '.', '.', '#', '#', '#', '#', '#' },
--               { '#', '.', '.', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#' },
--               { '#', '.', '.', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#' },
--               { '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#' },
--               { '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '#', '#', '#', '.', '#' },
--               { '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', '#', '.', '#' },
--               { '^', '#', '#', '#', '#', '#', '#', '#', '#', '.', '.', '.', '#', '.', '#' },
--               { '.', '.', '.', '.', '.', '.', '#', '.', '#', '.', '.', '.', '#', '.', '#' },
--               { '.', '.', '.', '.', '.', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#' },
--               { '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.' },
--               { '.', '.', '.', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '.' },
--               { '.', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.' },
--               { '.', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.' },
--               { '.', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.' },
--               { '.', '.', '.', '.', '#', '#', '#', '#', '#', '.', '.', '.', '.', '.', '.', } }

printCameraView()

function findRobotPos()
    for y, row in pairs(cameraView)
    do
        for x, cell in pairs(row)
        do
            if cell ~= '#' and cell ~= '.' then
                return { y = y, x = x }
            end
        end
    end
end

local robotPos = findRobotPos()
print("robot at: x:", robotPos["x"], " y:", robotPos["y"])

-- find movement sequence
local sequence = {}
local directionToChar = { up = '^', down = 'v', left = '<', right = '>' }
local directions = { up = { y = -1, x = 0 }, down = { y = 1, x = 0 }, left = { y = 0, x = -1 }, right = { y = 0, x = 1 } }
local onTheLeft = { up = { y = 0, x = -1 }, down = { y = 0, x = 1 }, left = { y = 1, x = 0 }, right = { y = -1, x = 0 } }
local onTheRight = { up = { y = 0, x = 1 }, down = { y = 0, x = -1 }, left = { y = -1, x = 0 }, right = { y = 1, x = 0 } }
local turnLeft = { up = '<', down = '>', left = 'v', right = '^' }
local turnRight = { up = '>', down = '<', left = '^', right = 'v' }
while true
do
    local dir
    if cameraView[robotPos["y"]][robotPos["x"]] == directionToChar["up"] then
        dir = "up"
    elseif cameraView[robotPos["y"]][robotPos["x"]] == directionToChar["down"] then
        dir = "down"
    elseif cameraView[robotPos["y"]][robotPos["x"]] == directionToChar["left"] then
        dir = "left"
    elseif cameraView[robotPos["y"]][robotPos["x"]] == directionToChar["right"] then
        dir = "right"
    end
    if robotPos["y"] + directions[dir]["y"] <= #cameraView and -- move forward
            robotPos["y"] + directions[dir]["y"] >= 1 and
            robotPos["x"] + directions[dir]["x"] <= #cameraView[1] and
            robotPos["x"] + directions[dir]["x"] >= 1 and
            cameraView[robotPos["y"] + directions[dir]["y"]][robotPos["x"] + directions[dir]["x"]] == '#' then
        if tonumber(sequence[#sequence]) == nil then
            table.insert(sequence, 1)
        else
            sequence[#sequence] = sequence[#sequence] + 1
        end
        --table.insert(sequence, 1)
        cameraView[robotPos["y"]][robotPos["x"]] = '#'
        robotPos = { y = robotPos["y"] + directions[dir]["y"], x = robotPos["x"] + directions[dir]["x"] }
        cameraView[robotPos["y"]][robotPos["x"]] = directionToChar[dir]
    elseif robotPos["y"] + onTheLeft[dir]["y"] >= 1 and -- turn left
            robotPos["y"] + onTheLeft[dir]["y"] <= #cameraView and
            robotPos["x"] + onTheLeft[dir]["x"] >= 1 and
            robotPos["x"] + onTheLeft[dir]["x"] <= #cameraView[1] and
            cameraView[robotPos["y"] + onTheLeft[dir]["y"]][robotPos["x"] + onTheLeft[dir]["x"]] == '#' then
        table.insert(sequence, 'L')
        cameraView[robotPos["y"]][robotPos["x"]] = turnLeft[dir]
    elseif robotPos["y"] + onTheRight[dir]["y"] >= 1 and -- turn right
            robotPos["y"] + onTheRight[dir]["y"] <= #cameraView and
            robotPos["x"] + onTheRight[dir]["x"] >= 1 and
            robotPos["x"] + onTheRight[dir]["x"] <= #cameraView[1] and
            cameraView[robotPos["y"] + onTheRight[dir]["y"]][robotPos["x"] + onTheRight[dir]["x"]] == '#' then
        table.insert(sequence, 'R')
        cameraView[robotPos["y"]][robotPos["x"]] = turnRight[dir]
    else
        break
    end
end

for k, _ in pairs(sequence) -- map to strings
do
    sequence[k] = tostring(sequence[k])
end
for i = #sequence, 1, -2 do
    -- join in pairs
    sequence[i - 1] = sequence[i - 1] .. sequence[i]
    table.remove(sequence, i)
end

print(table.concat(sequence, ","))

function table.copy(org)
    return { table.unpack(org) }
end

function table.unique(arr)
    local set = {}
    for _, v in pairs(arr) do
        set[v] = true
    end
    local result = {}
    for k, _ in pairs(set) do
        result[#result + 1] = k
    end
    return result
end

function checkFinished(arr)
    if #arr >= 20 then
        return false
    end
    local unique = table.unique(arr)
    if #unique > 3 then
        return false
    end
    for _, v in pairs(unique) do
        if #v > 20 then
            return false
        end
    end
    return true
end

function joinSequences(arr)
    for i = #arr, 2, -1 do
        local current = table.copy(arr)
        local joined = current[i - 1] .. current[i]
        if #joined <= 20 then
            current[i - 1] = joined
            table.remove(current, i)
            local joins = 1
            for j = i - 1, 2, -1 do
                -- join rest
                if current[j - 1] .. current[j] == joined then
                    current[j - 1] = joined
                    table.remove(current, j)
                    joins = joins + 1
                end
            end
            if joins >= 2 then
                -- sequences are probably repeated
                if checkFinished(current) then
                    return current
                end
                local result = joinSequences(current)
                if result then
                    return result
                end
            end
        end
    end
    return nil
end
local joined = joinSequences(sequence)
print("joined", table.concat(joined, ","))
local routines = table.unique(joined)
print("unique:", table.concat(routines, ", "))

local seqToName = {}
local nameToSeq = {}
local names = { 'A', 'B', 'C' }
for k, v in pairs(routines) do
    seqToName[v] = names[k]
    nameToSeq[names[k]] = v
end
local encoded = {}
for _, v in pairs(joined) do
    encoded[#encoded + 1] = seqToName[v]
end

print("Main routine", table.concat(encoded))
for k, v in pairs(seqToName) do
    print(v, ":", k)
end

-- read memory again
memory = readMemory()
memory[1] = 2
local robotInput = table.concat(encoded, ",") .. "\n"
for _, name in pairs(names) do
    local s = nameToSeq[name]
    s = string.gsub(s, "R", "R,")
    s = string.gsub(s, "L", "L,")
    s = string.gsub(s, "%d+", function(d)
        return d .. ","
    end)
    robotInput = robotInput .. string.sub(s, 1, #s - 1) .. "\n"
end
robotInput = robotInput .. 'n\n'
print(robotInput)
local charId = 1
function provideChar()
    local result = string.byte(string.sub(robotInput, charId))
    charId = charId + 1
    return result
end
function printOutput(value)
    if value <= 255 then
        io.write(string.char(value))
    else
        print("Answer:", value)
    end
end
print("Started movement")
ic = IntegerComputer:new(memory, provideChar, printOutput)
ic:run()