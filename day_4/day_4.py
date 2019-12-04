def double_number(number):
    for i in range(len(number) - 1):
        if number[i] == number[i + 1]:
            return True
    return False


def increasing(number):
    for i in range(len(number) - 1):
        if number[i] > number[i + 1]:
            return False
    return True


def correct(number):
    return increasing(number) and double_number(number)


def main():
    start, end = map(int, open('day_4.in').read().split('-'))
    counter = 0
    for number in range(start, end):
        number = str(number)
        if correct(number):
            counter += 1
    print(counter)


if __name__ == '__main__':
    assert correct('111111')
    assert not correct('223450')
    assert not correct('123789')
    main()
