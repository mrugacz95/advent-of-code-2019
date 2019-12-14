package pl.mrugacz95.day11

class Day11Part2 {
    static void main(String[] args) {
        Map<Integer, String> memory = new File("day_11.in")
                .readLines()[0]
                .split(',')
                .toList()
                .withIndex()
                .collectEntries { code, idx ->
                    [(idx.toLong()): code]
                }
        def robot = new Robot()
        robot.paint(Robot.Color.WHITE)
        def colorConsumed = false
        def ic = new IntegerComputer(memory, {
            return robot.readColor() != Robot.Color.WHITE ? 0 : 1
        }, {
            if (!colorConsumed) {
                robot.paint(it == 0 ? Robot.Color.BLACK : Robot.Color.WHITE)
                colorConsumed = true
            } else {
                robot.rotate(it == 0 ? Robot.Rotate.LEFT : Robot.Rotate.RIGHT)
                robot.moveForward()
                colorConsumed = false
            }
        })
        ic.run()
        robot.printBoard()
        println(robot.countPainted())
    }
}
