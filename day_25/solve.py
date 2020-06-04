import enum
import itertools
import re
import subprocess
TOOK_PATTERN = re.compile(r'\nYou take the (?P<item_took>(.*))\.\n\nCommand\?\n')
ROOM_PATTERN = re.compile(r'(You take the (?P<item_took>(.*))\.)|'
                          r'((?P<alert>(A loud, robotic voice says "Alert! Droids on this ship are'
                             r' (heavier|lighter) than the detected value!" and you are ejected back '
                             r'to the checkpoint.)\n)?'
                             r'\n\n\n== (?P<room_name>[a-zA-Z ]+) ==\n'
                             r'(?P<room_desc>.*)\n\n'
                             r'Doors here lead:\n'
                             r'(?P<paths>(- (east|west|south|north)\n)+)\n'
                             r'(Items here:\n'
                             r'(?P<items>(- [a-z ]+)\n)+\n)?'
                             r'Command\?\n)')

POSSIBLE_ITEMS = ['cake',
                  'tambourine',
                  'mutex',
                  'klein bottle',
                  'dark matter', ]
CERTAINLY_FORBIDDEN_ITEMS = ['photons', 'infinite loop', 'molten lava', 'giant electromagnet']
ALL_ITEMS = POSSIBLE_ITEMS + CERTAINLY_FORBIDDEN_ITEMS
FORBIDDEN_ITEMS = []


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
        s = ROOM_PATTERN.search(msg)
        if s is None:
            print(f"Regex didnt matched: {msg}")
            return None
        if s.groupdict()['item_took']:
            return None
        name = s.group('room_name')
        desc = s.group('room_desc')
        paths = strip_list(s.group('paths'))
        paths = [Path[p.upper()] for p in paths]
        items = []
        if s.groupdict()['items']:
            items = strip_list(s.group('items'))
        return Room(current_pos, name, desc, paths, items)

    def __str__(self):
        return f"{self.id}. {self.name}: pos={self.pos} items={self.items} name={self.name} desc={self.desc} paths={self.paths}"

    def __gt__(self, other):
        return self.id > other.id

    def __eq__(self, other):
        return self.name == other.name

    def update(self, room):
        self.items = room.items


def strip_list(list):
    return list.replace('- ', '').strip().split('\n')


def choose_direction(room_map, pos):
    visited = set()

    def dfs(room_map, pos, distance):
        if pos not in room_map:
            return None, distance
        room = room_map[pos]
        if room.id in visited:
            return None, None
        visited.add(room.id)
        commands = []
        for path in room.paths:
            path.from_position(pos)
            new_pos = path.from_position(pos)
            comm, dist = dfs(room_map, new_pos, distance + 1)
            if dist is not None:
                commands.append((path, dist))
        if commands:
            return sorted(commands, key=lambda x: x[1])[0]
        return None, None

    command, _ = dfs(room_map, pos, 0)
    return command


def print_map(current_pos, room_map):
    if not room_map:
        return
    x_min = min(map(lambda x: x[1], list(room_map.keys()))) - 1
    x_max = max(map(lambda x: x[1], list(room_map.keys()))) + 2
    y_min = min(map(lambda x: x[0], list(room_map.keys()))) - 1
    y_max = max(map(lambda x: x[0], list(room_map.keys()))) + 2
    table = [[None for _ in range(x_min, x_max)] for _ in range(y_min, y_max)]
    for y in range(y_min, y_max):
        for x in range(x_min, x_max):
            if (y, x) in room_map:
                table[y - y_min][x - x_min] = room_map[(y, x)]
    for row in table:
        for state in ['up', 'mid', 'bot']:
            for room in row:
                if room is None:
                    print('    ', end='')
                elif state == 'up':
                    print(' ', end='')
                    print('||' if Path.NORTH in room.paths else '  ', end='')
                    print(' ', end='')
                elif state == 'mid':
                    print('=' if Path.WEST in room.paths else ' ', end='')
                    print(f'{room.id:02}', end='')
                    print('=' if Path.EAST in room.paths else ' ', end='')
                elif state == 'bot':
                    print(' ', end='')
                    print('||' if Path.SOUTH in room.paths else '  ', end='')
                    print(' ', end='')
            print()
    for room in sorted(room_map.values()):
        print(room)


def read_stdin(proc, debug=False):
    message = ""
    while not message.endswith("Command?\n"):
        new_line = proc.stdout.readline().decode()
        if debug:
            print(new_line, end='')
        message += new_line
    return message


def play(debug):
    Room.id = 1
    room_map = {}
    inventory = []
    pos = (0,0)
    proc = subprocess.Popen("./day_25", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    print(f"Started: allowed items {set(ALL_ITEMS).difference(FORBIDDEN_ITEMS)}")
    while True:
        message = read_stdin(proc, debug)
        if "you are ejected back to the checkpoint" in message:
            alert = ROOM_PATTERN.search(message).group('alert')
            print(f"Game over. Max room: {Room.id}\n{alert}")
            proc.kill()
            return False
        if TOOK_PATTERN.match(message):
            room = room_map[pos]
        elif ROOM_PATTERN.match(message):
            room = Room.from_message(pos, message)
        else:
            print("Message didn't match")
            print(message)
            proc.kill()
            return False
        if room is None:
            if pos in room_map:
                room = room_map[pos]
            else:
                proc.kill()
                return False
        if room not in room_map.values():
            room_map[pos] = room
        else:
            def find_pos(r):
                for k, v in room_map.items():
                    if v == r:
                        return k
            pos = find_pos(room)
            room_map[pos].update(room)
        command = choose_direction(room_map, pos)
        if command is None:
            raise RuntimeError("No command available")
        if debug:
            print(f"Position: {pos}")
            print(f"Inventory: {inventory}")
        for item in room.items:
            if item not in FORBIDDEN_ITEMS:
                command = "take " + room.items[0]
                inventory.append(room.items.pop())
                break
        else:
            pos = command.from_position(pos)
            command = command.name.lower()
        if debug:
            print(f"Bot move: {command}")
        command = command + "\n"
        proc.stdin.write(command.encode())
        proc.stdin.flush()
        if debug:
            print_map(pos, room_map)


def all_combinations():
    for i in range(len(POSSIBLE_ITEMS) + 1):
        yield from itertools.combinations(POSSIBLE_ITEMS, i)


def main():
    global FORBIDDEN_ITEMS
    for forbidden in all_combinations():
        FORBIDDEN_ITEMS = CERTAINLY_FORBIDDEN_ITEMS + list(forbidden)
        win = play(False)
        if win:
            print("WON")
            break
        # break # this is not correct solution, but you can see gameplay from one round


if __name__ == '__main__':
    main()
