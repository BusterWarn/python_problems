import re

def main():
    lines = []
    with open("input.txt") as fp:
        line = fp.readline()
        while line:
            line = re.sub("\s", "", line)
            if (line.isdigit()):
                lines.append(int(line))
            else:
                print("ERROR: Line is not an integer: '%s'" %(line))
            line = fp.readline()

    problem_1(lines)
    problem_2(lines)


def problem_1(lines):
    increment = 0
    last_depth = lines[0]
    for i in range(len(lines)):
        if lines[i] > last_depth:
            increment = increment + 1
        last_depth = lines[i]
    print("Problem 1 Increment: %d" %(increment))


def problem_2(lines):
    increment = 0
    last_depth = lines[0] + lines[1] + lines[2]
    for i in range(len(lines) - 2):
        depth = lines[i] + lines[i + 1] + lines[i + 2]
        if depth > last_depth:
            increment = increment + 1
        last_depth = depth
    print("Problem 2 Increment: %d" %(increment))

main()
