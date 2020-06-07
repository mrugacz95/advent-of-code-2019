from math import sqrt, log
from random import random

import matplotlib.pyplot as plt


def is_correct(g):
    for node, neighbours in enumerate(g):
        for neighbour in neighbours:
            if node not in g[neighbour]:
                return False
    return True


def repair(g):
    new_g = [[] for _ in range(len(g))]
    for node, neighbours in enumerate(g):
        for neighbour in neighbours:
            if node not in new_g[neighbour]:
                new_g[neighbour].append(node)
            if neighbour not in new_g[node]:
                new_g[node].append(neighbour)
    return new_g


def plot_graph(positions, forces=None):
    x = [p[0] for p in positions]
    y = [p[1] for p in positions]
    plt.scatter(x, y)
    for node, neighbours in enumerate(graph):
        plt.text(positions[node][0], positions[node][1], str(node))
        for n in neighbours:
            plt.plot([x[node], x[n]], [y[node], y[n]], c='black', alpha=0.5)
    if forces:
        for f_id, f in enumerate(forces):
            plt.plot([x[f_id], x[f_id] + f[0]], [y[f_id], y[f_id] + f[1]], c='red', linewidth=2)
    plt.show()


def draw(g):
    if not is_correct(g):
        g = repair(g)
    positions = [(random(), random()) for _ in range(len(g))]
    plt.title("Before")
    plot_graph(positions)
    c_rep = 0.1
    c_spring = 0.1
    iters = 100
    ideal_length = 0.3

    def delta_func(x):
        return 1 / x

    def dist(n1, n2):
        x1, y1 = positions[n1]
        x2, y2 = positions[n2]
        return sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)

    def unit(n1, n2):
        x1, y1 = positions[n1]
        x2, y2 = positions[n2]
        d = dist(n1, n2)
        return (x1 - x2) / d, (y1 - y2) / d

    def mul(v, value):
        x, y = v
        return x * value, y * value

    def add(v1, v2):
        return v1[0] + v2[0], v1[1] + v2[1]

    def f_repulsive(u, v):
        return mul(unit(u, v), c_rep / (dist(v, u) ** 2))

    def f_attractive(u, v):
        return mul(unit(v, u), c_spring * log(dist(u, v) / ideal_length))

    forces = []
    for i in range(1, iters):
        cool_factor = 1  # delta_func(i)
        forces = []
        for node in range(len(g)):
            force = (0, 0)
            for other in range(len(g)):
                if other == node:
                    continue
                if other in g[node]:
                    f = f_attractive
                else:
                    f = f_repulsive
                force = add(force, f(node, other))
            forces.append(force)
        for n_id, force in enumerate(forces):
            positions[n_id] = add(mul(force, cool_factor), positions[n_id])
    plt.title("After")
    plot_graph(positions, forces)


if __name__ == '__main__':
    graph = [
        [1, 2, 3],  # 0
        [0, 4, 5, 6],  # 1
        [7],  # 2
        [8, 9],  # 3
        [10, 11],  # 4
        [],  # 5
        [12],  # 6
        [8],  # 7
        [14, 13],  # 8
        [10],  # 9
        [6, 13],  # 10
        [3, 12],  # 11
        [],  # 12
        [12],  # 13
        [13]  # 14
    ]
    draw(graph)
    graph = [
        [1],
        [2],
        [3],
        [4],
        [0],
        [4],
        [5],
        [6],
        [7],
        [4, 8]
    ]
    draw(graph)
    graph = [
        [1],
        [2],
        [3],
        [4],
        [5],
        [6],
        [7],
        [8],
        [9],
        [0]
    ]
    draw(graph)
