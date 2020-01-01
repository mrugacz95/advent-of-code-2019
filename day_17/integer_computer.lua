local IntegerComputer = { running = true,
                          pointer = 0,
                          relativeBase = 0,
                          memory = {} }

function IntegerComputer:new (memory, input, output)
    local instance = {}
    setmetatable(instance, self)
    self.__index = self
    self.memory = memory
    self.input = input
    self.output = output
    self.running = true
    self.relativeBase = 0
    return instance
end

function IntegerComputer:read(position)
    if self.memory[position + 1] == nil then
        self.memory[position + 1] = 0
    end
    return self.memory[position + 1]
end

function IntegerComputer:write(offset, value)
    local mode = self:getMode(offset)
    if mode == 0 then
        self.memory[self:read(self.pointer + offset) + 1] = value
    elseif mode == 1 then
        self.memory[pointer + offset + 1] = value
    elseif mode == 2 then
        self.memory[self.relativeBase + self:read(self.pointer + offset) + 1] = value
    end
end

function IntegerComputer:getMode(offset)
    local mode = self.memory[self.pointer + 1] // 100
    for _ = 1, offset - 1, 1
    do
        mode = mode // 10
    end
    return mode % 10
end

function IntegerComputer:getParam(offset)
    local mode = self:getMode(offset)
    if mode == 0 then
        return self:read(self:read(self.pointer + offset))
    elseif mode == 1 then
        return self:read(self.pointer + offset)
    elseif mode == 2 then
        return self:read(self.relativeBase + self:read(self.pointer + offset))
    end
    error("Unknown mode in getParam: " .. mode)
end

function IntegerComputer:run()
    while self.running
    do
        local opcode = self:read(self.pointer) % 100
        if opcode == 1 then
            local a = self:getParam(1)
            local b = self:getParam(2)
            self:write(3, a + b)
            self.pointer = self.pointer + 4
        elseif opcode == 2 then
            local a = self:getParam(1)
            local b = self:getParam(2)
            self:write(3, a * b)
            self.pointer = self.pointer + 4
        elseif opcode == 3 then
            local value = self.input()
            self:write(1, value)
            self.pointer = self.pointer + 2
        elseif opcode == 4 then
            local value = self:getParam(1)
            self.output(value)
            self.pointer = self.pointer + 2
        elseif opcode == 5 then
            local a = self:getParam(1)
            local b = self:getParam(2)
            if a ~= 0 then
                self.pointer = b
            else
                self.pointer = self.pointer + 3
            end
        elseif opcode == 6 then
            local a = self:getParam(1)
            local b = self:getParam(2)
            if a == 0 then
                self.pointer = b
            else
                self.pointer = self.pointer + 3
            end
        elseif opcode == 7 then
            local a = self:getParam(1)
            local b = self:getParam(2)
            value = 0
            if a < b then
                value = 1
            end
            self:write(3, value)
            self.pointer = self.pointer + 4
        elseif opcode == 8 then
            local a = self:getParam(1)
            local b = self:getParam(2)
            value = 0
            if a == b then
                value = 1
            end
            self:write(3, value)
            self.pointer = self.pointer + 4
        elseif opcode == 9 then
            local a = self:getParam(1)
            self.relativeBase = self.relativeBase + a
            self.pointer = self.pointer + 2
        elseif opcode == 99 then
            self.running = false
        else
            error("Unknown opcode: " .. opcode)
        end
    end
end

function IntegerComputer:printMemory()
    for k, v in pairs(self.memory)
    do
        io.write(k, ":", v, " ")
    end
    print()
end

return IntegerComputer