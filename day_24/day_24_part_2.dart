import 'dart:io';
import 'dart:math';

const BOARD_SIZE = 5;

class Coord {
  int x, y, lvl;

  Coord(y, x, lvl) {
    this.x = x;
    this.y = y;
    this.lvl = lvl;
  }

  @override
  bool operator ==(other) {
    return other.x == this.x && other.y == this.y && other.lvl == this.lvl;
  }

  @override
  int get hashCode {
    return this.lvl * 100 + this.x * 10 + this.y;
  }

  operator +(other) {
    return new Coord(this.y + other.y, this.x + other.x, this.lvl + other.lvl);
  }

  @override
  String toString() {
    return "<${this.y},${this.x}: ${this.lvl}>";
  }
}

getLvlRange(Map<Coord, bool> board) {
  var maxLvl = 0;
  var minLvl = 0;
  board.forEach((c, val) {
    if (val) {
      maxLvl = max(maxLvl, c.lvl);
      minLvl = min(minLvl, c.lvl);
    }
  });
  return [minLvl, maxLvl];
}

int countNeighbours(Map<Coord, bool> board, Coord c) {
  var neighbours = [];
  // vertical neighbours
  if (c.y == 0) {
    neighbours.add(new Coord(1, 2, c.lvl - 1));
    neighbours.add(new Coord(1, c.x, c.lvl));
  } else if (c.y == 4) {
    neighbours.add(new Coord(3, 2, c.lvl - 1));
    neighbours.add(new Coord(3, c.x, c.lvl));
  } else if (c.y == 3 && c.x == 2) {
    for (var x = 0; x < BOARD_SIZE; x++) {
      neighbours.add(new Coord(4, x, c.lvl + 1));
    }
    neighbours.add(new Coord(4, 2, c.lvl));
  } else if (c.y == 1 && c.x == 2) {
    for (var x = 0; x < BOARD_SIZE; x++) {
      neighbours.add(new Coord(0, x, c.lvl + 1));
    }
    neighbours.add(new Coord(0, 2, c.lvl));
  } else {
    neighbours.add(new Coord(c.y + 1, c.x, c.lvl));
    neighbours.add(new Coord(c.y - 1, c.x, c.lvl));
  }

  // horizontal neighbours
  if (c.x == 0) {
    neighbours.add(new Coord(2, 1, c.lvl - 1));
    neighbours.add(new Coord(c.y, 1, c.lvl));
  } else if (c.x == 4) {
    neighbours.add(new Coord(2, 3, c.lvl - 1));
    neighbours.add(new Coord(c.y, 3, c.lvl));
  } else if (c.y == 2 && c.x == 3) {
    for (var y = 0; y < BOARD_SIZE; y++) {
      neighbours.add(new Coord(y, 4, c.lvl + 1));
    }
    neighbours.add(new Coord(2, 4, c.lvl));
  } else if (c.y == 2 && c.x == 1) {
    for (var y = 0; y < BOARD_SIZE; y++) {
      neighbours.add(new Coord(y, 0, c.lvl + 1));
    }
    neighbours.add(new Coord(2, 0, c.lvl));
  } else {
    neighbours.add(new Coord(c.y, c.x - 1, c.lvl));
    neighbours.add(new Coord(c.y, c.x + 1, c.lvl));
  }
  var count = 0;
  for (var n in neighbours) {
    if (board.containsKey(n) && board[n]) count++;
  }
  return count;
}

growBoard(Map<Coord, bool> board) {
  var minmax = getLvlRange(board);
  for (var y = 0; y < BOARD_SIZE; y++) {
    for (var x = 0; x < BOARD_SIZE; x++) {
      var coord = new Coord(y, x, minmax[1] + 1);
      if (!board.containsKey(coord) && (y != 2 || x != 2)) {
        board[coord] = false;
      }
    }
  }
  for (var y = 0; y < BOARD_SIZE; y++) {
    for (var x = 0; x < BOARD_SIZE; x++) {
      var coord = new Coord(y, x, minmax[0] - 1);
      if (!board.containsKey(coord) && (y != 2 || x != 2)) {
        board[coord] = false;
      }
    }
  }
}

Map<Coord, bool> step(Map<Coord, bool> board) {
  Map<Coord, bool> newBoard = new Map<Coord, bool>();
  growBoard(board);
  board.forEach((c, value) {
    var counter = countNeighbours(board, c);
    newBoard[c] = counter == 1 || (!board[c] && (counter == 2));
  });
  return newBoard;
}

prettyPrintBoard(Map<Coord, bool> board) {
  var minamx = getLvlRange(board);
  for (var lvl = minamx[0]; lvl <= minamx[1]; lvl++) {
    print("l ${lvl}");
    for (var y = 0; y < BOARD_SIZE; y++) {
      for (var x = 0; x < BOARD_SIZE; x++) {
        var coord = new Coord(y, x, lvl);
        if (!board.containsKey(coord)) {
          stdout.write("?");
        } else {
          stdout.write(board[coord] ? '#' : '.');
        }
      }
      print('');
    }
  }
  print('');
}

Map<Coord, bool> readInput(var filename) {
  Map<Coord, bool> result = new Map();
  new File('day_24.in')
      .readAsStringSync()
      .split('\n')
      .map((line) => line
          .split('')
          .map<bool>(((x) => x == '#' ? true : false))
          .toList()
          .asMap())
      .toList()
      .asMap()
      .forEach((y, line) => line.forEach((x, value) => {
            if (y != 2 || x != 2)
              {result.putIfAbsent(new Coord(y, x, 0), () => value)}
          }));
  return result;
}

int countBugs(Map<Coord, bool> board) {
  var sum = 0;
  board.forEach((coord, value) {
    if (value) sum++;
  });
  return sum;
}

main() {
  var board = readInput('day_24.in');
  prettyPrintBoard(board);
  for (var i = 0; i < 200; i++) {
    board = step(board);
  }
//  prettyPrintBoard(board);
  print(countBugs(board));
}
