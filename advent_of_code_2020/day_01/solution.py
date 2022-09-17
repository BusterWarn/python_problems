import re

def main():
    target = 2020
    lines = []
    with open("input") as fp:
        line = fp.readline()
        while line:
            line = re.sub("\s", "", line)
            if (line.isdigit() and int(line) <= target):
                lines.append(int(line))
            line = fp.readline()

    tuple = findTupleThatSumsTo(lines, target)
    print(tuple)
    print(tuple[0])

    for i in range(len(lines)):
        equals = findTupleThatSumsTo(lines, target - lines[i])
        if equals != -1:
            print("Target found: %d * %d * %d = %d" %(lines[i], equals[0], equals[1], lines[i] * equals[0] * equals[1]))




def findTupleThatSumsTo(list, sum):
    for i in range(len(list) - 1):
        for j in range(i + 1, len(list)):
            if (list[i] + list[j] == sum):
                print("To find", sum)
                return (list[i], list[j])
    return -1

main()