# Source https://codeforces.com/blog/entry/72593
import re
from functools import reduce

M = 119315717514047  # assumption: M is prime
k = 101741582076661
x = 2020


def parse_move(line):
    if re.match("deal into new stack", line):
        return -1, -1
    result = re.search("cut (-?\d+)", line)
    if result:
        return 1, -int(result.group(1))
    result = re.search("deal with increment (\d+)", line)
    if result:
        return int(result.group(1)), 0


def compose(move1, move2):
    a, b = move1
    c, d = move2
    return a * c % M, (b * c + d) % M


def evaluate(move, card):  # for first part
    a, b = move
    return (a * card + b) % M


def inverse_mod(a, m):
    return pow_mod(a, m - 2, m)


def pow_mod(x, n, m):
    y = 1
    while n > 0:
        if n % 2 != 0:
            y = y * x % m
        n = n // 2
        x = x * x % m
    return y


def F_repeated_k_inverse(F, k, x):
    a, b = F
    a_k = pow_mod(a, k, M)
    A = a_k
    B = b * (1 - a_k) * inverse_mod(1 - a, M)
    return (x - B) * inverse_mod(A, M) % M


def main():
    moves = []
    with open('day_22.in') as file:
        for line in file:
            moves.append(parse_move(line))
    F = list(reduce(compose, moves))
    # print(evaluate(F, 2019)) # for first part
    ans = F_repeated_k_inverse(F, k, x)
    print(ans)


if __name__ == '__main__':
    main()
