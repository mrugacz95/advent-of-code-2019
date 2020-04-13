import 'dart:io';
import 'dart:math';

List<List<bool>> step(List<List<bool>> board) {
  List<List<bool>> newBoard = new List.generate(5, (i) => List(5));
  var neighbours = [
    Point(-1, 0),
    Point(1, 0),
    Point(0, -1),
    Point(0, 1),
  ];
  board.asMap().forEach((y, row) => row.asMap().forEach((x, value) {
        var counter = 0;
        neighbours.asMap().forEach((i, delta) {
          Point point = Point(x + delta.x, y + delta.y);
          if (point.y >= 0 &&
              point.y < board.length &&
              point.x >= 0 &&
              point.x < board[0].length) // is in board range
          {
            if (board[point.y][point.x]) {
              counter++;
            }
          }
        });
        newBoard[y][x] = counter == 1 || (!board[y][x] && (counter == 2));
      }));
  return newBoard;
}

prettyPrintBoard(List<List<bool>> board) {
  for (var row in board) {
    for (var value in row) {
      stdout.write(value ? '#' : '.');
    }
    print('');
  }
}

List<List<bool>> readInput(var filename) {
  return new File('day_24.in')
      .readAsStringSync()
      .split('\n')
      .map((line) => line
          .split('')
          .map<bool>(((x) => x == '#' ? true : false))
          .toList()
          .cast<bool>())
      .toList(growable: false)
      .cast<List<bool>>();
}

int biodiversity(List<List<bool>> board) {
  int power = 1;
  int sum = 0;
  for (var row in board) {
    for (var value in row) {
      if (value) sum += power;
      power *= 2;
    }
  }
  return sum;
}

main() {
  var board = readInput('day_24.in');
  var visited = Set<int>();
  int value;
  do {
    value = biodiversity(board);
    board = step(board);
  } while (visited.add(value));
  print(value);
}
