def calculate_fuel(mass):
    return mass // 3 - 2


def main():
    print(sum(calculate_fuel(mass) for mass in map(int, open('day_1.in').read().split())))


if __name__ == '__main__':
    main()
