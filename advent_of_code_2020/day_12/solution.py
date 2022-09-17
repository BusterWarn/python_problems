import re

def main():
    actions = []
    with open("input") as fp:
        line = fp.readline()
        while line:
            line = re.sub("\r|\n", "", line)
            action = re.sub("\d", "", line), int(re.sub("[A-Z]", "", line))
            actions.append(action)
            line = fp.readline()
    
    print(" ----- Part 1 -----")
    dir = 'E'
    x_cord = 0
    y_cord = 0
    for action in actions:
        if action[0] == 'E' or action[0] == 'N' or  action[0] == 'W' or action[0] == 'S':
            x_cord, y_cord = move(action[0], x_cord, y_cord, action[1])
        elif action[0] == 'R' or action[0] == 'L':
            dir = rotate_ferry(dir, action[0], action[1])
        elif action[0] == 'F':
            x_cord, y_cord = move(dir, x_cord, y_cord, action[1])
    print_ferry(x_cord, y_cord, dir)
    
    print("\n ------ Part 2 ------")
    x_cord = 0
    y_cord = 0
    wp_x_cord = 10
    wp_y_cord = 1
    for action in actions:
        if action[0] == 'E' or action[0] == 'N' or  action[0] == 'W' or action[0] == 'S':
            wp_x_cord, wp_y_cord = move(action[0], wp_x_cord, wp_y_cord, action[1])
        elif action[0] == 'R' or action[0] == 'L':
            wp_x_cord, wp_y_cord= rotate_waypoint(action[0], action[1], wp_x_cord, wp_y_cord)
        elif action[0] == 'F':
            x_cord += action[1] * wp_x_cord
            y_cord += action[1] * wp_y_cord
    print_ferry(x_cord, y_cord, dir)

def rotate_ferry(current_dir, rot_way, rot_degrees):
    rot_degrees = rot_degrees % 360
    if rot_way == 'L':
        rot_degrees = 360 - rot_degrees
    directions = ['E', 'S', 'W', 'N']
    dir_idx = 0
    while (directions[dir_idx] != current_dir):
        dir_idx += 1
    
    if rot_degrees == 90:
        return directions[(dir_idx + 1) % 4]
    if rot_degrees == 180:
        return directions[(dir_idx + 2) % 4]
    if rot_degrees == 270:
        return directions[(dir_idx + 3) % 4]
    return directions[dir_idx]

def rotate_waypoint(rot_way, rot_degrees, x_cord, y_cord):
    rot_degrees = rot_degrees % 360
    if rot_way == 'L':
        rot_degrees = 360 - rot_degrees
    
    if rot_degrees == 90:
        return y_cord, -x_cord
    if rot_degrees == 180:
        return -x_cord, -y_cord
    if rot_degrees == 270:
        return -y_cord, x_cord
    return x_cord, y_cord

def move(dir, x_cord, y_cord, len):
    if dir == 'E':
        x_cord += len
    elif dir == 'N':
        y_cord += len
    elif dir == 'W':
        x_cord -= len
    elif dir == 'S':
        y_cord -= len
    return x_cord, y_cord

def print_ferry(x_cord, y_cord, dir = 'N/A'):
    print("Boat: [%d %d]. Dir: %s. Manhattan distance: %d"
        %(x_cord, y_cord, dir, abs(x_cord) + abs(y_cord)))

def abs(value):
    if (value < 0):
        return -value
    return value

main()