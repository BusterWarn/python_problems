import copy

def main():
    lines = []
    filepath = "input"
    with open(filepath) as fp:
        line = fp.readline()
        while line:
            lines.append(line)
            line = fp.readline()
    
    seats = []
    for line in lines:
        row_of_seats = []
        for seat in line:
            if seat != "\n":
                row_of_seats.append(seat)
        seats.append(row_of_seats)
    
    stabile = False
    
    while not stabile:
        last_seats = copy.deepcopy(seats)
        new_seats = copy.deepcopy(update_seats_adjacent(seats))
        if last_seats == new_seats:
            break
    
    print("Adjacent based seats stabilized. Occupied seats:", count_occupied_seats(seats))

    stabile = False
    while not stabile:
        last_seats = copy.deepcopy(seats)
        new_seats = copy.deepcopy(update_seats_visible(seats))
        if last_seats == new_seats:
            break
    print("Visible based seats stabilized. Occupied seats:", count_occupied_seats(seats))



def update_seats_adjacent(seats):
    new_seat_arrangement = copy.deepcopy(seats)

    for i in range(len(seats)):
        for j in range(len(seats[0])):
            if seats[i][j] == 'L' and check_occupied_neighbours(seats, i, j) == 0:
                new_seat_arrangement[i][j] = '#'
            elif seats[i][j] == '#' and check_occupied_neighbours(seats, i, j) >= 4:
                new_seat_arrangement[i][j] = 'L'

    return new_seat_arrangement


def update_seats_visible(seats):
    new_seat_arrangement = copy.deepcopy(seats)

    for i in range(len(seats)):
        for j in range(len(seats[0])):
            if seats[i][j] == 'L' and check_occupied_neighbours(seats, i, j) == 0:
                new_seat_arrangement[i][j] = '#'
            elif seats[i][j] == '#' and check_visible_neighbours(seats, i, j) >= 5:
                new_seat_arrangement[i][j] = 'L'

    return new_seat_arrangement
    

def check_occupied_neighbours(seats, row, col):
    occupied_neighbours = 0
    for i in range(row - 1, row + 2):
        if col == 3 and row == 0 and False: print("now j")
        for j in range(col - 1, col + 2):
            # current seat?
            if i == row and j == col:
                continue
            # row out of bound?
            elif i < 0 or i >= len(seats):
                continue
            # col out of bound?
            elif j < 0 or j >= len(seats[0]):
                continue
            elif seats[i][j] == '#':
                occupied_neighbours += 1
    return occupied_neighbours
    

def check_visible_neighbours(seats, row, col):
    occupied_neighbours = 0
    for i in range(row - 1, row + 2):
        if col == 3 and row == 0 and False: print("now j")
        for j in range(col - 1, col + 2):
            #print("\n\ni: %d, j: %d len: %d - %d" %(i, j, len(seats), len(seats[0])))
            if col == 3 and row == 0 and False: print("[%d %d]" %(i, j), end="")
            # current seat?
            if i == row and j == col:
                if col == 3 and row == 0 and False: print("current seat")
                continue
            # row out of bound?
            elif i < 0 or i >= len(seats):
                if col == 3 and row == 0 and False: print("row out of bound")
                continue
            # col out of bound?
            elif j < 0 or j >= len(seats[0]):
                if col == 3 and row == 0 and False: print("col out of bound")
                continue
            elif seats[i][j] == '#':
                if col == 3 and row == 0 and False: print("bingo")
                occupied_neighbours += 1
            elif seats[i][j] == '.':
                if col == 3 and row == 0 and False: print("no seat")
            else:
                if col == 3 and row == 0 and False: print("   [%d][%d] = %s" %(i, j, seats[i][j]))
        #print()
    #print(occupied_neighbours)
    return occupied_neighbours
    

def count_occupied_seats(seats):
    occupied_seats = 0
    for i in range(len(seats)):
        for j in range(len(seats[0])):
            if seats[i][j] == '#':
                occupied_seats += 1
    return occupied_seats

def count_visible_seats(seats, row, col):
    visible_seats = 0
    if top_left_visible(seats, row, col):
        visible_seats += 1
    if top_visible(seats, row, col):
        visible_seats += 1
    if top_right_visible(seats, row, col):
        visible_seats += 1
    if right_visible(seats, row, col):
        visible_seats += 1
    if bottom_right_visible(seats, row, col):
        visible_seats += 1
    if bottom_visible(seats, row, col):
        visible_seats += 1
    if bottom_left_visible(seats, row, col):
        visible_seats += 1
    if left_visible(seats, row, col):
        visible_seats += 1
    
    return visible_seats


def top_left_visible(seats, row, col):

    row_to_check = row
    col_to_check = col
    while 1 != 2:
        row_to_check -= 1
        col_to_check -= 1
        if row_to_check < 0 or col_to_check < 0:
            break
        elif seats[row_to_check][col_to_check] == '#':
            return False
    return True

def top_visible(seats, row, col):

    row_to_check = row
    while 1 != 2:
        row_to_check -= 1
        if row_to_check < 0:
            break
        elif seats[row_to_check][col] == '#':
            return False
    return True

def top_right_visible(seats, row, col):
    
    row_to_check = row
    col_to_check = col
    while 1 != 2:
        row_to_check -= 1
        col_to_check += 1
        if row_to_check < 0 or col_to_check >= len(seats[0]):
            break
        elif seats[row_to_check][col_to_check] == '#':
            return False
    return True

def right_visible(seats, row, col):
    
    col_to_check = col
    while 1 != 2:
        col_to_check += 1
        if col_to_check >= len(seats[0]):
            break
        elif seats[row][col_to_check] == '#':
            return False
    return True

def bottom_right_visible(seats, row, col):

    row_to_check = row
    col_to_check = col
    while 1 != 2:
        row_to_check += 1
        col_to_check += 1
        if row_to_check >= len(seats) or col_to_check >= len(seats[0]):
            break
        elif seats[row_to_check][col_to_check] == '#':
            return False
    return True

def bottom_visible(seats, row, col):

    row_to_check = row
    while 1 != 2:
        row_to_check += 1
        if row_to_check >= len(seats):
            break
        elif seats[row_to_check][col] == '#':
            return False
    return True

def bottom_left_visible(seats, row, col):

    row_to_check = row
    col_to_check = col
    while 1 != 2:
        row_to_check += 1
        col_to_check -= 1
        if row_to_check >= len(seats) or col_to_check < 0:
            break
        elif seats[row_to_check][col_to_check] == '#':
            return False
    return True

def left_visible(seats, row, col):

    col_to_check = col
    while 1 != 2:
        col_to_check -= 1
        if  col_to_check < 0:
            break
        elif seats[row][col_to_check] == '#':
            return False
    return True

# For debug purposes.
def print_seats(seats):
    print()
    for i in range(len(seats)):
        for j in range(len(seats[0])):
            print(seats[i][j], end="")
        print()
main()
