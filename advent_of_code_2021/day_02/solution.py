import re

def main():
    lines = []
    with open("input.txt") as fp:
        line = fp.readline()
        while line:
            line = re.sub("\n", "", line)
            tuple = re.split("\s", line)
            if not tuple[1].isdigit():
                print("ERROR WITH INPUT LINE:", line)
                return
            lines.append(tuple)
            line = fp.readline()

    problem_1(lines)
    problem_2(lines)


def problem_1(lines):
    x = 0
    y = 0
    for i in range(len(lines)):
        direction = lines[i][0]
        value = int(lines[i][1])
        if direction == "forward":
            x += value
        elif direction == "down":
            y += value
        elif direction == "up":
            y -= value
    print("Problem 1: x = %d, y = %d, x*y = %d" %(x, y, x * y))

def problem_2(lines):
    x = 0
    aim = 0
    depth = 0
    for i in range(len(lines)):
        direction = lines[i][0]
        value = int(lines[i][1])
        if direction == "forward":
            x += value
            depth += value * aim
        elif direction == "down":
            aim += value
        elif direction == "up":
            aim -= value
        else:
            print("Invalid input: '%s'. Returning")
            return
    print("Problem 2: aim = %d, x = %d, depth = %d, x*depth = %d" %(aim, x, depth, x * depth))

main()
