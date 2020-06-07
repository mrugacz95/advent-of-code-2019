import copy
import random


def opposite(directory):
    return {
        'north': 'south',
        'east': 'west',
        'south': 'north',
        'west': 'east'
    }.get(directory)


def move_position(position, direction):
    pass
    dy, dx = {
        'north': (-1, 0),
        'south': (1, 0),
        'west': (0, -1),
        'east': (0, 1),
    }.get(direction)
    y, x = position
    return y + dy, x + dx


def is_correct(room_map):
    for node, neigh in room_map.items():
        for d, n in neigh.items():
            if d not in ['north', 'east', 'south', 'west']:
                return False, f'Wrong direction {d}'
            if n is None:
                continue
            if opposite(d) not in room_map[n]:
                return False, f'One direction connection: from {node} to {n} but not from {n} to {node}'
            if room_map[n][opposite(d)] != node:
                return False, f'Wrong node connected in opposite direction: from {node} it is {n}, but from {n} it is {room_map[n][opposite(d)]}'
    return True, None


def _display(positions, corridors, room_map):
    x_min = min(map(lambda x: x[1], list(positions.keys())))
    x_max = max(map(lambda x: x[1], list(positions.keys()))) + 1
    y_min = min(map(lambda x: x[0], list(positions.keys())))
    y_max = max(map(lambda x: x[0], list(positions.keys()))) + 1

    for y in range(y_min, y_max):
        for state in ['up', 'mid', 'bot']:
            for x in range(x_min, x_max):
                if (y, x) not in positions:
                    print('    ', end='')
                else:
                    room_name = positions[(y, x)]
                    paths = room_map[room_name]
                    if state == 'up':
                        print(' ', end='')
                        print('||' if 'north' in paths else '  ', end='')
                        print(' ', end='')
                    elif state == 'mid':
                        print('=' if 'west' in paths else ' ', end='')
                        if room_name in corridors:
                            if 'north' in room_map[room_name] and 'south' in room_map[room_name]:
                                print('║║', end='')
                            else:
                                print('≡≡', end='')
                        else:
                            print(f'{room_name:02}', end='')
                        print('=' if 'east' in paths else ' ', end='')
                    elif state == 'bot':
                        print(' ', end='')
                        print('||' if 'south' in paths else '  ', end='')
                        print(' ', end='')
            print()


def draw(room_map, start_node=None):
    correct, msg = is_correct(room_map)
    if not correct:
        raise RuntimeError(f"Invalid map: {msg}")  # todo try to fix map
    positions = {}
    if start_node is None:
        start_node = random.choice(list(room_map.keys()))
    visited = set()

    def get_next_node_name():
        return max(room_map.keys()) + 1

    corridors = set()  # corridors will not be displayed as rooms

    def move_room(pos, direction, parent_name):
        room_name = positions.pop(pos, None)
        if room_name is None:  # room not in positions yet, not need to move
            return
        new_pos = move_position(pos, direction)
        if new_pos in positions:  # new position is also occupied
            move_room(new_pos, direction, room_name)
        # fix neighbours
        for neigh_dir, neigh in room_map[room_name].items():
            if neigh_dir != opposite(direction) \
                    and neigh != parent_name:  # do not fix parent and neighbours from direction
                neigh_pos = move_position(pos, neigh_dir)
                move_room(neigh_pos, direction, room_name)
        if opposite(direction) in room_map[room_name] \
                and room_map[room_name][opposite(direction)] != parent_name:
            # insert corridor
            corridor = get_next_node_name()
            positions[pos] = corridor
            room_map[corridor] = {
                direction: room_name,
                opposite(direction): room_map[room_name][opposite(direction)]
            }
            detached_room_name = room_map[room_name].pop(opposite(direction))
            room_map[room_name][opposite(direction)] = corridor
            room_map[detached_room_name].pop(direction)
            room_map[detached_room_name][direction] = corridor
            corridors.add(corridor)
        positions[new_pos] = room_name

    def find_position(room_name):
        for k, v in positions.items():
            if room_name == v:
                return k
        raise RuntimeError(f"Unexpected error. Position not found: {room_name} {positions}")

    def insert_node(pos, room_name, from_dir=None):
        if room_name is None:
            return
        if pos in positions and positions[pos] != room_name:  # pos already taken, need to move
            prev_neigh_pos = move_position(pos, opposite(from_dir))
            move_room(prev_neigh_pos, opposite(from_dir), room_name)
            pos = move_position(pos, opposite(from_dir))
        visited.add(room_name)
        positions[pos] = room_name
        # _display(positions, corridors, room_map) # debug
        for n_dir, neighbour in room_map[room_name].items():
            if neighbour in visited:
                continue
            pos = find_position(room_name)  # update position as it could be changed by children
            new_pos = move_position(pos, n_dir)
            insert_node(new_pos, neighbour, from_dir=n_dir)

    insert_node((0, 0), start_node)
    _display(positions, corridors, room_map)


def main():
    room_maps = [
        {
            1: {'north': 2},
            2: {'south': 1,
                'west': 3},
            3: {'east': 2,
                'south': 4},
            4: {'south': 13,
                'east': 5,
                'north': 3},
            5: {'west': 4,
                'south': 6},
            6: {'north': 5,
                'east': 7},
            7: {'west': 6,
                'north': 8},
            8: {'south': 7,
                'west': 9,
                'east': 10},
            9: {'east': 8},
            10: {'west': 8,
                 'south': 11},
            11: {'north': 10,
                 'east': 12},
            12: {'west': 11},
            13: {'north': 4,
                 'east': 14},
            14: {'west': 13}
        },
        {
            1: {'north': 2},
            2: {'south': 1,
                'west': 3},
            3: {'east': 2,
                'south': 4},
            4: {'north': 3,
                'east': 5,
                'south': 9},
            5: {'west': 4,
                'south': 6},
            6: {'north': 5,
                'east': 7},
            7: {'west': 6,
                'north': 8},
            8: {'south': 7},
            9: {'north': 4,
                'south': 10},
            10: {'north': 9,
                 'east': 11},
            11: {'west': 10}
        },
        {
            2: {'south': 3},
            3: {'north': 2,
                'east': 4},
            4: {'west': 3,
                'north': 5},
            5: {'west': 6,
                'south': 4},
            6: {'east': 5,
                'south': 7},
            7: {'north': 6,
                'east': 8},
            8: {'west': 7,
                'north': 9},
            9: {'south': 8,
                'west': 10},
            10: {'east': 9},
        }]
    for room_map in room_maps:
        rooms = list(room_map.keys())
        for k in rooms:
            draw(copy.deepcopy(room_map), k)


if __name__ == '__main__':
    main()
