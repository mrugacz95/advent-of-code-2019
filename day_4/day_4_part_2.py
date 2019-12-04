def double_number(number):
    counter = 0
    for i in range(len(number) - 1):
        if number[i] == number[i + 1]:
            counter += 1
        else:
            if counter == 1:
                return True
            counter = 0
    return counter == 1


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
    assert correct('112233')
    assert not correct('123444')
    assert correct('111122')
    main()
