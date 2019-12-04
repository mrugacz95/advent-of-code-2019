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
            print('x' if grid[(x, y)][0] is not None or grid[(x, y)][1] is not None else '.', end='')
        print()


def main():
    lines = []
    for line in open('day_3.in').read().split():
        lines.append(line.split(','))
    grid = defaultdict(lambda: [None, None])
    for idx, line in enumerate(lines):
        pos = (0, 0)
        line_steps = 1
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
                if grid[pos][idx] is None:
                    grid[pos][idx] = line_steps
                line_steps += 1
    least_steps = math.inf
    for k, v in grid.items():
        if v[0] is not None and v[1] is not None:
            steps = v[0] + v[1]
            if steps < least_steps:
                least_steps = steps
    # print_grid(grid)
    print(least_steps)


if __name__ == '__main__':
    main()
