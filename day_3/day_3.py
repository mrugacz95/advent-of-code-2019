import math
from collections import defaultdict


def print_grid(grid):
    min_x = min([k[0] for k, v in grid.items()])
    min_y = min([k[1] for k, v in grid.items()])
    max_x = max([k[0] for k, v in grid.items()])
    max_y = max([k[1] for k, v in grid.items()])
    for y in range(min_y, max_y):
        for x in range(min_x, max_x):
            if x == 0 and y == 0:
                print('O', end='')
                continue
            print('x' if grid[(x, y)] else '.', end='')
        print()


def main():
    lines = []
    for line in open('day_3.in').read().split():
        lines.append(line.split(','))
    grid = defaultdict(int)
    for idx, line in enumerate(lines):
        pos = (0, 0)
        for segment in line:
            d = segment[0]
            length = int(segment[1:])
            move = {
                'R': (1, 0),
                'L': (-1, 0),
                'U': (0, 1),
                'D': (0, -1),
            }.get(d)
            for i in range(length):
                pos = (pos[0] + move[0], pos[1] + move[1])
                if grid[pos] == 0:
                    grid[pos] = idx + 1
                elif grid[pos] != idx + 1:
                    grid[pos] = 3

    closest = math.inf
    for k, v in grid.items():
        if v >= 3:
            new_dist = abs(k[0]) + abs(k[1])
            if new_dist < closest:
                closest = new_dist
    # print_grid(grid)
    print(closest)


if __name__ == '__main__':
    main()
