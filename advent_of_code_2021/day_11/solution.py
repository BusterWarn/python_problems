import re

class octopus:
    def __init__(self, x, y, energy):
        self.x = x
        self.y = y
        self.energy = energy
        self.has_flashed = False

    def __eq__(self, other):
        if self.x == other.x and self.y == other.y:
            return True
        return False
    
    def __str__(self):
        return "[%d, %d]: %d - %s" %(self.x, self.y, self.energy, self.has_flashed)

    def __hash__(self):
        return hash((self.x, self.y))

def main():
    octopuses = []
    with open("input.txt") as fp:
        line = fp.readline()
        x = 0
        while line:
            line = re.sub("\n", "", line)
            if len(line) == 0:
                break
            line = [int(s) for s in line]
            row_of_octopuses = [None] * len(line)
            for y in range(len(line)):
                row_of_octopuses[y] = octopus(x, y, line[y])

            octopuses.append(row_of_octopuses)
            x += 1
            line = fp.readline()

    if False:
        problem_1(octopuses)
    else:
        problem_2(octopuses)

def problem_1(octopuses):
    flash_count = 0
    for i in range(100):
        will_flash = tick_the_time_by_one_tick(octopuses)
        flash_count += flash(octopuses, will_flash)
    
    print("Problem 1 flash count: %d" %(flash_count))

def problem_2(octopuses):
    num_octopus = 0
    for row in octopuses:
        num_octopus += len(row)

    iterations = 0
    while 69 != 1337 and 420 != "smoke_week_every_day":
        iterations += 1
        will_flash = tick_the_time_by_one_tick(octopuses)
        flash_count = flash(octopuses, will_flash)
        if flash_count == num_octopus:
            print("All %d octopus flash at step %d" %(flash_count, iterations))
            break

def tick_the_time_by_one_tick(octopuses):
    will_flash = set()
    for row in octopuses:
        for oc in row:
            oc.energy += 1
            oc.has_flashed = False
            if oc.energy > 9:
                will_flash.add(oc)
    return will_flash

def flash(octopuses, will_flash):
    flash_count = 0
    while len(will_flash) > 0:
        oc = will_flash.pop()
        adj_octopuses = find_adjacent_octopuses(octopuses, oc)
        for adj_oc in adj_octopuses:
            if adj_oc.energy > 9 or adj_oc.has_flashed:
                continue
            adj_oc.energy += 1
            if adj_oc.energy > 9:
                will_flash.add(adj_oc)
        oc.energy = 0
        oc.has_flashed = True
        flash_count += 1
    return flash_count

def find_adjacent_octopuses(octopuses, octopus):
    adjacent = []
    x = octopus.x
    y = octopus.y

    # Top left
    if x != 0 and y != 0:
        adjacent.append(octopuses[x - 1][y  -1])
    # Above
    if x != 0:
        adjacent.append(octopuses[x - 1][y])
    # Top right
    if x != 0 and y < len(octopuses[x]) -1:
        adjacent.append(octopuses[x - 1][y + 1])
    # Left
    if y != 0:
        adjacent.append(octopuses[x][y - 1])
    # Right
    if y < len(octopuses[x]) -1:
        adjacent.append(octopuses[x][y + 1])
    # Bottom left
    if x < len(octopuses) - 1 and y != 0:
        adjacent.append(octopuses[x + 1][y - 1])
    # Below
    if x < len(octopuses) - 1:
        adjacent.append(octopuses[x + 1][y])
    # Bottom right
    if x < len(octopuses) -1 and y < len(octopuses[x]) - 1:
        adjacent.append(octopuses[x + 1][y + 1])
    
    return adjacent

def print_octopuses(octopus):
    print()
    for row in octopus:
        for oc in row:
            if oc.energy == 9:
                print("X", end="")
            else:
                print(oc.energy, end="")
        print()

main()