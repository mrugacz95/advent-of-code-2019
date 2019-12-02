def calculate_fuel(mass):
    if mass <= 0:
        return 0
    else:
        base_fuel = max(0, mass // 3 - 2)
        return base_fuel + calculate_fuel(base_fuel)


def main():
    data = map(int, open('day_1.in').read().split())
    fuel_needed = sum(calculate_fuel(mass) for mass in data)
    print(fuel_needed)


if __name__ == '__main__':
    main()
