import enum
import itertools
import re
import subprocess
from collections import defaultdict

from draw_labirynth import draw

TOOK_PATTERN = re.compile(r'\nYou take the (?P<item_took>(.*))\.\n\nCommand\?\n')
ALERT_PATTERN = re.compile(r'(?P<message>(A loud, robotic voice says .*))\n')
ROOM_PATTERN = re.compile(r'\n\n\n== (?P<room_name>[a-zA-Z ]+) ==\n'
                          r'(?P<room_desc>.*)\n\n'
                          r'Doors here lead:\n'
                          r'(?P<paths>(- (east|west|south|north)\n)+)\n'
                          r'(Items here:\n'
                          r'(?P<items>(- [a-z ]+)\n)+\n)?'
                          r'Command\?\n')

CERTAINLY_FORBIDDEN_ITEMS = ['photons', 'infinite loop', 'molten lava', 'giant electromagnet', 'escape pod']
FORBIDDEN_ITEMS = CERTAINLY_FORBIDDEN_ITEMS.copy()  # + ['cake', 'mutex', 'klein bottle', 'fuel cell']


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

    def opposite(self):
        return {
            self.NORTH: self.SOUTH,
            self.SOUTH: self.NORTH,
            self.EAST: self.WEST,
            self.WEST: self.EAST,
        }[self]


class Room:
    id = 1

    def __init__(self, name, desc, paths, items):
        self.items = items
        self.name = name
        self.desc = desc
        self.paths = paths
        self.id = Room.id

    @staticmethod
    def from_message(msg):
        s = ROOM_PATTERN.search(msg)
        if s is None:
            print(f"Regex didn't matched: {msg}")
            return None
        name = s.group('room_name')
        desc = s.group('room_desc')
        paths = strip_list(s.group('paths'))
        paths = {Path[p.upper()]: None for p in paths}
        items = []
        if s.groupdict()['items']:
            items = strip_list(s.group('items'))
        return Room(name, desc, paths, items)

    def __str__(self):
        return f"{self.name} items={self.items} name={self.name} desc={self.desc} paths={self.paths}"

    def __hash__(self):
        return hash(self.name)

    def update(self, room):
        self.items = room.items


def strip_list(list):
    return list.replace('- ', '').strip().split('\n')


def choose_direction(available_rooms, current_room):
    visited = set()
    dist = {}  # distance to closest unknown room
    next_room_dir = {}

    def dfs(room_name, distance):
        if room_name in visited:
            return dist[room_name]
        visited.add(room_name)
        dist[room_name] = float('inf')  # init dist if dead end
        for path, neigh in available_rooms[room_name].paths.items():  # check neighbours
            if neigh is None:
                dist[room_name] = 0
                next_room_dir[room_name] = path
                break
            else:
                neigh_dist = dfs(neigh, distance + 1)
                if neigh_dist < dist[room_name]:
                    dist[room_name] = neigh_dist
                    next_room_dir[room_name] = path
        return dist[room_name]

    dfs(current_room, 0)
    return next_room_dir.get(current_room, None)  # if next not set return None


def print_map(available_rooms):
    drawing_struct = {}
    names = {name: id for id, name in enumerate(available_rooms.keys())}
    for room_name, room in available_rooms.items():
        drawing_struct[names[room_name]] = {}
        for path, neigh_room in available_rooms[room_name].paths.items():
            drawing_struct[names[room_name]][path.name.lower()] = None if not neigh_room else names[neigh_room]
    draw(drawing_struct, 0)
    for room_name, room in available_rooms.items():
        print(f"{names[room_name]}. {room}")


def read_stdin(proc, debug=False):
    message = ""
    while not message.endswith("Command?\n") and 'airlock' not in message.lower():
        new_line = proc.stdout.readline().decode()
        if debug:
            print(new_line, end='')
        message += new_line
    return message


def play(debug):
    available_rooms = {}
    inventory = []
    proc = subprocess.Popen("./day_25", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    print(f"Started: forbidden items {FORBIDDEN_ITEMS}")
    room = None
    last_room = None
    last_command = None
    walked = True
    while True:
        message = read_stdin(proc, debug)
        if ALERT_PATTERN.search(message):
            alert_msg = ALERT_PATTERN.search(message).group('message')
            print(f"Game over. {alert_msg}")
            proc.kill()
            if 'Analysis complete!' in message:
                print(message)
                return True, inventory
            return False, inventory
        if TOOK_PATTERN.match(message):
            new_item = TOOK_PATTERN.search(message).group('item_took')
            inventory.append(new_item)
            room.items.remove(new_item)
        elif ROOM_PATTERN.search(message):
            last_room = room
            room = Room.from_message(message)
            if room.name in available_rooms:
                available_rooms[room.name].items = room.items  # update items only
            else:
                available_rooms[room.name] = room  # update whole room
            if walked and last_room is not None:
                available_rooms[last_room.name].paths[Path[last_command.upper()]] = room.name
                available_rooms[room.name].paths[Path[last_command.upper()].opposite()] = last_room.name
        else:
            print("Message didn't match")
            print(message)
            proc.kill()
            return False, inventory
        if debug:
            print(f"Current room: {room.name}")
            print(f"Inventory: {inventory}")
        for item in room.items:  # is possible to take something?
            if item not in FORBIDDEN_ITEMS:
                command = "take " + room.items[0]
                walked = False
                break
        else:
            walked = True
            command = choose_direction(available_rooms, room.name)
            if command is None:
                raise RuntimeError("No command available")
            command = command.name.lower()
        if debug:
            print(f"Bot move: {command}")
        last_command = command
        command = command + "\n"
        proc.stdin.write(command.encode())
        proc.stdin.flush()
        if debug:
            print_map(available_rooms)


def all_combinations(arr):
    for i in range(len(arr) + 1):
        yield from itertools.combinations(arr, i)


def main():
    global FORBIDDEN_ITEMS
    _, all_possible_items = play(False)
    print('Possible items:', all_possible_items)
    for possible_items in all_combinations(all_possible_items):
        FORBIDDEN_ITEMS = CERTAINLY_FORBIDDEN_ITEMS + list(possible_items)
        win, _ = play(False)
        if win:
            print("WON")
            break


if __name__ == '__main__':
    main()
