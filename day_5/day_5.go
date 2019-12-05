package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func readOpCode(opcode string) (string, string, string) {
	// left pad with zeros
	for i := len(opcode); i < 4; i++ {
		opcode = "0" + opcode
	}
	return opcode[0:1], opcode[1:2], opcode[2:]
}

func atoi(s string) int {
	i, _ := strconv.Atoi(s)
	return i
}

func loadParam(pointer int, code []string, offset int, readMode string) int {
	var param string
	if readMode == "0" {
		param = code[atoi(code[pointer+offset])]
	} else {
		param = code[pointer+offset]
	}
	return atoi(param)
}

func main() {
	dat, _ := ioutil.ReadFile("day_5.in")
	reader := bufio.NewReader(os.Stdin)
	code := strings.Split(string(dat), ",")
	pointer := 0
	running := true
	for running {
		var B, C, DE = readOpCode(code[pointer])
		var result int
		switch DE {
		case "01":
			param1, param2, param3 := loadParam(pointer, code, 1, C), loadParam(pointer, code, 2, B), loadParam(pointer, code, 3, "1")
			result = param1 + param2
			code[param3] = strconv.Itoa(result)
			pointer += 4
		case "02":
			param1, param2, param3 := loadParam(pointer, code, 1, C), loadParam(pointer, code, 2, B), loadParam(pointer, code, 3, "1")
			result = param1 * param2
			code[param3] = strconv.Itoa(result)
			pointer += 4
		case "03":
			param1 := loadParam(pointer, code, 1, "1")
			text, _ := reader.ReadString('\n')
			text = strings.TrimSuffix(text, "\n")
			code[param1] = text
			pointer += 2
		case "04":
			param1 := loadParam(pointer, code, 1, C)
			fmt.Println(param1)
			pointer += 2
		case "99":
			running = false
		default:
			fmt.Println("Error")
			running = false
		}
	}
	fmt.Println()
}
