import enum
import re
import subprocess
import numpy

COMMAND_PATTERN = re.compile(r'\n\n\n== (?P<room_name>[a-zA-Z ]+) ==\n'
                             r'(?P<room_desc>.*)\n\n'
                             r'Doors here lead:\n'
                             r'(?P<paths>(- (east|west|south|north)\n)+)\n'
                             r'(Items here:\n'
                             r'(?P<items>(- [a-z]+)\n)+\n)?'
                             r'Command\?\n')


class Path(enum.Enum):
    NORTH = 1
    SOUTH = 2
    EAST = 3
    WEST = 4

    def to_delta(self):
        return {
            'NORTH': (-1, 0),
            'SOUTH': (1, 0),
            'EAST': (0, 1),
            'WEST': (0, -1)
        }[self.name]

    def from_position(self, pos):
        return tuple(sum(x) for x in zip(pos, self.to_delta()))


class Room:
    id = 1

    def __init__(self, pos, name, desc, paths, items):
        self.pos = pos
        self.items = items
        self.name = name
        self.desc = desc
        self.paths = paths
        self.id = Room.id
        Room.id += 1

    @staticmethod
    def from_message(current_pos, msg):
        s = COMMAND_PATTERN.search(msg)
        if s is None:
            print(f"unrecognised message:\n{msg}")
            return None
        name = s.group('room_name')
        desc = s.group('room_desc')
        paths = strip_list(s.group('paths'))
        paths = [Path[p.upper()] for p in paths]
        items = []
        if s.groupdict()['items']:
            items = strip_list(s.group('items'))
            print('Room items: ', items)
        return Room(current_pos, name, desc, paths, items)

    def __str__(self):
        return f"{self.id}. {self.name}:\n pos={self.pos}\n items={self.items}\n name={self.name}\n desc={self.desc}\n paths={self.paths}"

    def __gt__(self, other):
        return self.id > other.id

def strip_list(list):
    return list.replace('- ', '').strip().split('\n')


def choose_command(room_map, pos):
    if pos not in room_map:
        return None, True
    for path in room_map[pos]:
        path.from_position(pos)
        new_pos = path.from_position(pos)
        command = choose_command(room_map, new_pos)
        if command[1]:
            return path, True
    return None, False


def print_map(current_pos, room_map):
    if not room_map:
        return
    x_min = min(map(lambda x: x[1], list(room_map.keys()))) - 1
    x_max = max(map(lambda x: x[1], list(room_map.keys()))) + 2
    y_min = min(map(lambda x: x[0], list(room_map.keys()))) - 1
    y_max = max(map(lambda x: x[0], list(room_map.keys()))) + 2
    table = [['_' for _ in range(x_min, x_max)] for _ in range(y_min, y_max)]
    for y in range(y_min, y_max):
        for x in range(x_min, x_max):
            if (y, x) not in room_map:
                continue
            else:
                table[y - y_min][x- x_min] = (room_map[(y, x)].id)
                for path in room_map[(y, x)].paths:
                    neighbour = path.from_position((y, x))
                    if neighbour in room_map:
                        continue
                    else:
                        n_y, n_x = neighbour
                        table[n_y - y_min][n_x- x_min] = '?'

    for row in table:
        for cell in row:
            print(cell, end='')
        print()
    for room in sorted(room_map.values()):
        print(room)


def main():
    room_map = {}
    pos = (0, 0)
    proc = subprocess.Popen("./day_25", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    print("Started")
    while True:
        message = ""
        while not message.endswith("Command?\n"):
            message += proc.stdout.readline().decode()
        print(message)
        room = Room.from_message(pos, message)
        if room is None:
            break
        room_map[pos] = room
        print_map(pos, room_map)
        command = input("Input: ")
        if command.upper() in Path.__members__:
            pos = Path[command.upper()].from_position(pos)
        command = command + "\n"
        proc.stdin.write(command.encode())
        proc.stdin.flush()
    proc.kill()


if __name__ == '__main__':
    main()
