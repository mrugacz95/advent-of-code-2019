package pl.mrugacz95.day9

import java.io.File
import java.util.*

fun readFromMemory(position: Long, memory: MutableMap<Long, String>): String {
    if (!memory.containsKey(position)) {
        memory[position] = "0"
    }
    return memory[position] ?: error("Memory not set at $position")
}

fun getMode(offset: Int, opcode: Int): Int {
    var mode = opcode / 100
    for (i in 1 until offset) {
        mode /= 10
    }
    mode %= 10
    return mode
}


fun getParam(offset: Int, pointer: Long, memory: MutableMap<Long, String>, mode: Int, relativeBase: Long): Long {
    return when (mode) {
        0 -> readFromMemory(readFromMemory((pointer + offset), memory).toLong(), memory).toLong() // position
        1 -> readFromMemory((pointer + offset), memory).toLong() // immediate
        2 -> readFromMemory(
            relativeBase + readFromMemory((pointer + offset), memory).toLong(),
            memory
        ).toLong() // relative
        else -> error("Unknown position mode: $mode")
    }
}

fun saveValue(
    offset: Int,
    pointer: Long,
    memory: MutableMap<Long, String>,
    mode: Int,
    relativeBase: Long,
    value: Long
) {
    when (mode) {
        0 -> memory[readFromMemory((pointer + offset), memory).toLong()] = value.toString()
        1 -> memory[pointer + offset] = value.toString()
        2 -> memory[relativeBase + readFromMemory((pointer + offset), memory).toLong()] = value.toString()
    }
}

fun main(args: Array<String>) {
    val memory = File("day_9.in")
        .readLines()
        .first()
        .split(",")
        .mapIndexed { idx, data -> idx.toLong() to data }
        .toMap()
        .toMutableMap()
    var pointer = 0L
    var relativeBase = 0L
    val reader = Scanner(System.`in`)
    var running = true
    while (running) {
        val opcode = readFromMemory(pointer, memory).toInt()
        when (opcode % 100) {
            1 -> {
                val a = getParam(1, pointer, memory, getMode(1, opcode), relativeBase)
                val b = getParam(2, pointer, memory, getMode(2, opcode), relativeBase)
                saveValue(3, pointer, memory, getMode(3, opcode), relativeBase, a + b)
                pointer += 4
            }
            2 -> {
                val a = getParam(1, pointer, memory, getMode(1, opcode), relativeBase)
                val b = getParam(2, pointer, memory, getMode(2, opcode), relativeBase)
                saveValue(3, pointer, memory, getMode(3, opcode), relativeBase, a * b)
                pointer += 4
            }
            3 -> {
                val input = reader.nextLong()
                saveValue(1, pointer, memory, getMode(1, opcode), relativeBase, input)
                pointer += 2
            }
            4 -> {
                val a = getParam(1, pointer, memory, getMode(1, opcode), relativeBase)
                println(a)
                pointer += 2
            }
            5 -> {
                val a = getParam(1, pointer, memory, getMode(1, opcode), relativeBase)
                val b = getParam(2, pointer, memory, getMode(2, opcode), relativeBase)
                if (a != 0L) {
                    pointer = b
                } else {
                    pointer += 3
                }
            }
            6 -> {
                val a = getParam(1, pointer, memory, getMode(1, opcode), relativeBase)
                val b = getParam(2, pointer, memory, getMode(2, opcode), relativeBase)
                if (a == 0L) {
                    pointer = b
                } else {
                    pointer += 3
                }
            }
            7 -> {
                val a = getParam(1, pointer, memory, getMode(1, opcode), relativeBase)
                val b = getParam(2, pointer, memory, getMode(2, opcode), relativeBase)
                saveValue(3, pointer, memory, getMode(3, opcode), relativeBase, if (a < b) 1L else 0L)
                pointer += 4
            }
            8 -> {
                val a = getParam(1, pointer, memory, getMode(1, opcode), relativeBase)
                val b = getParam(2, pointer, memory, getMode(2, opcode), relativeBase)
                saveValue(3, pointer, memory, getMode(3, opcode), relativeBase, if (a == b) 1L else 0L)
                pointer += 4
            }
            9 -> {
                val a = getParam(1, pointer, memory, getMode(1, opcode), relativeBase)
                relativeBase += a
                pointer += 2
            }
            99 -> {
                running = false
            }
            else -> {
                println("Wrong opcode: $opcode")
                running = false
            }
        }
    }
}
