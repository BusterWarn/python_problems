import re

def main():
    lines = []
    row_length = 0
    col_length = 0
    with open("input.txt") as fp:
        line = fp.readline()
        while line:
            new_line = process_line(line)
            row_length, col_length = evaluate_line_length(new_line, row_length, col_length)
            lines.append(new_line)
            line = fp.readline()

    draw_lines(lines, row_length, col_length)

def is_diagonal(line):
    if line[0] == line[2] or line[1] == line[3]:
        return False
    return True

def draw_lines(lines, row_length, col_length):
    map = [[0 for x in range(row_length + 1)] for y in range(col_length + 1)] 

    for i in range(len(lines)):
        if is_diagonal(lines[i]):
            draw_diagonal_line(lines[i], map)
        else:
            draw_straight_line(lines[i], map)

    evaluate_map(map)

def draw_straight_line(line, map):
    if (is_diagonal(line)):
        print("draw_straight_line: ERRROR LINE %s IS DIAGONAL" %(line))
    
    if line[0] != line[2]:
        x_max = line[0]
        x_min = line[2]
        if x_max < x_min:
            x_max = line[2]
            x_min = line[0]
        for i in range(x_min, x_max + 1):
            map[i][line[1]] += 1
    else:
        y_max = line[1]
        y_min = line[3]
        if y_max < y_min:
            y_max = line[3]
            y_min = line[1]
        for i in range(y_min, y_max + 1):
            map[line[0]][i] += 1


def draw_diagonal_line(line, map):
    if line[0] > line[2] and line[1] > line[3]:
        x_max = line[0]
        x_min = line[2]
        y_max = line[1]
        y_min = line[3]

        if x_max - x_min != y_max - y_min:
            print("ERROR WITH LINE:", line)
        
        for i in range(x_max - x_min + 1):
            map[x_min + i][y_min + i] += 1

    elif line[0] > line[2] and line[1] < line[3]:
        x_max = line[0]
        x_min = line[2]
        y_max = line[3]
        y_min = line[1]

        if x_max - x_min != y_max - y_min:
            print("ERROR WITH LINE:", line)

        for i in range(x_max - x_min + 1):
            map[x_min + i][y_max - i] += 1

    elif line[0] < line[2] and line[1] > line[3]:
        x_max = line[2]
        x_min = line[0]
        y_max = line[1]
        y_min = line[3]

        if x_max - x_min != y_max - y_min:
            print("ERROR WITH LINE:", line)
        
        for i in range(x_max - x_min + 1):
            map[x_max - i][y_min + i] += 1

    else:
        x_max = line[2]
        x_min = line[0]
        y_max = line[3]
        y_min = line[1]

        if x_max - x_min != y_max - y_min:
            print("ERROR WITH LINE:", line)
        
        for i in range(x_max - x_min + 1):
            map[x_max - i][y_max - i] += 1


def evaluate_map(map):
    nr_overlaps = 0
    for i in range(len(map)):
        for j in range(len(map[i])):
            if map[i][j] > 1:
                nr_overlaps += 1
    print("Overlaps", nr_overlaps)

def evaluate_line_length(line, row_length, col_length):
    if line[0] > row_length:
        row_length = line[0]
    if line[2] > row_length:
        row_length = line[2]
    if line[1] > col_length:
        col_length = line[1]
    if line[3] > col_length:
        col_length = line[3]
    return row_length, col_length

def process_line(line):
    line = re.sub("\n", "", line)
    line = re.sub(" -> ", ",", line)
    line = line.split(",")
    line = [int(s) for s in line]
    return line

main()