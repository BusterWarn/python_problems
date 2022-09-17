import re
import ast

MIN_DEPTH = 1
MAX_DEPTH = 4

def main():
    homework = []
    with open("input.txt") as fp:
        line = fp.readline()
        while line:
            line = re.sub("\n", "", line)
            if len(line) == 0:
                break
            homework.append(ast.literal_eval(line))
            line = fp.readline()
    
    snailfish_add(homework[0])

    #num = ast.literal_eval("[[[[[9,8],1],2],3],4]")
    #print(num)
    #reduce(num)
    #print(num)
    if 1 == 1:
        return

    for num in homework:
        print("\nReducing", num)
        ret = reduce([False, False], num)
        print("%s\t => \t %s" %(num, ret))

def snailfish_add(num_1 = [], num_2 = []):
    num_1 = ast.literal_eval("[[[[4,3],4],4],[7,[[8,4],9]]]")
    num_2 = ast.literal_eval("[1,1]")
    new_snailfish_num = [num_1, num_2]
    print("\nNEW %s\n\n" %(new_snailfish_num))
    print(new_snailfish_num)
    new_new_snailfish_num = reduce(new_snailfish_num)
    print("\n %s => %s" %(new_snailfish_num, new_new_snailfish_num))

def reduce(num, exploded = [False, False], depth = MIN_DEPTH):
    bottom_reached = True
    left = -1
    right = -1
    for part_num in num:
        print("reduce at depth: %d with num: %s. Left: %s Right: %s" %(depth, part_num, left, right))
        val = part_num
        if isinstance(part_num, list):
            bottom_reached = False
            if exploded[0] or exploded[1]:
                #print("Here it happens. part_num: ", part_num)
                val = part_num
            else:
                print(">> Depth: %d, left: %s, right: %s, Reduce FURTHER" %(depth, left, right))
                val = reduce(part_num, exploded, depth + 1)
        elif not isinstance(part_num, int):
            print("ERROR wrong type:", part_num)
            return "ERROR"
        
        if val == None:
            left = part_num

        if left == -1:
            left = val
        else:
            right = val
    
    print("<< DEPTH: %d LEFT: '%s', RIGHT: '%s', Bottom reached: %s" %(depth, left, right, bottom_reached))
    if bottom_reached and depth > MAX_DEPTH:
        print("BOTTOM REACHED - Depth:", depth, "Returning X  \t", [left, right], "Bottom reached at", depth)
        return [left, right]
    

    if isinstance(left, list) and isinstance(right, int):
        if not exploded[1]:
            print("EXPLODE at depth %d LEFT IS LIST. Left %s Right %s" %(depth, left, right))
            exploded[1] = True
            boom = left[1] + right
            if depth == MAX_DEPTH:
                print("Depth:", depth, "Returning 0.0\t", [left[0], [0, boom]])
                return [left[0], [0, boom]]
            else:
                print("Depth:", depth, "Returning 0.1\t", [left[0], boom])
                return [left[0], boom]
        else:
            if depth == MIN_DEPTH:
                print("Depth:", depth, "Returning 1  \t", [left[1], right])
                return [left[1], right]
            else:
                print("Depth:", depth, "Returning 1  \t", [left[0], [left[1], right]])
                return [left[0], [left[1], right]]

    elif isinstance(left, int) and isinstance(right, list):
        if not exploded[0]:
            print("EXPLODE at depth %d RIGHT IS LIST. Left %s Right %s" %(depth, left, right))
            exploded[0] = True
            boom = left + right[0]
            if depth == MAX_DEPTH:
                print("Depth:", depth, "Returning 2.0\t", [[boom, 0], right[1]])
                return [[boom, 0], right[1]]
            else:
                print("Depth:", depth, "Returning 2.1\t", [boom, right[1]])
                return [boom, right[1]]

        else:
            if depth == MIN_DEPTH:
                print("Depth:", depth, "Returning 3.0\t", [right[0], right[1]])
                return [left, right[0]]
            else:
                print("Depth:", depth, "Returning 3.1\t", [[left, right[0]], right[1]])
                return [[left, right[0]], right[1]]
    #elif isinstance(left, list) and isinstance(left[1], int) and isinstance(right, list) and isinstance(right[0], int):
    elif isinstance(left, list) and isinstance(right, list):
        print("THIS IS IT!!! EXPLODED [%s %s] Rightmost Int? %s" %(exploded[0], exploded[1], rightmost_is_integer(right)))
        if not exploded[1] and rightmost_is_integer(right):
            print("Del right 1: %s => %s" %(right, right[0]))
            del right[1]

        if exploded[0] and exploded[1]:
            print("Returning 0")
            return [left[0], [left[1] + right[0], right[1]]]
        elif exploded[0] and not exploded[1]:
            return [left, right[0]]
        else:
            print("Returning 1")
            return [[left[0], [left[1], right[0]]], right[1]]
    else:
        print("Invalid return. Depth: %d, Snailfish number: %s, left: %s, right: %s" %(depth, num, left, right))
        
    #print("Num:", num, "Depth:", depth, "First:", left, "Second", right)


    #if isinstance(first_num, int):
        #map.insert(0, first_num)
        #print("EXPLODE FROM right?")
    #elif first_num != None and isinstance(second_num, int):
        #map.insert(0, second_num)
        #print("EXPLODE FROM LEFT?. First:", first_num, "Second:", second_num)
        #num = 42
    
    #print("Depth:", depth, "Num:", num, "first:", first_num, "second:", second_num)
    print("Depth:", depth, "Returning 5  \tNone")
    return None

def get_leftmost(snailfish_number):
    if isinstance(snailfish_number, int):
        return snailfish_number
    elif isinstance(snailfish_number, list):
        return get_leftmost(snailfish_number[0])
    else:
        print("get_leftmost::ERROR %s not a snailfish number" %(snailfish_number))
        return "ERROR"

def get_rightmost(snailfish_number):
    if isinstance(snailfish_number, int):
        return snailfish_number
    elif isinstance(snailfish_number, list):
        return get_rightmost(snailfish_number[-1])
    else:
        print("get_rightmost::ERROR %s not a snailfish number" %(snailfish_number))
        return "ERROR"

def rightmost_is_integer(snailfish_number):
    if not isinstance(snailfish_number, list):
        return False
    if isinstance(snailfish_number[0], list) and isinstance(snailfish_number [1], int):
        return True
    elif isinstance(snailfish_number[0], int) and isinstance(snailfish_number[1], int):
        return False
    elif isinstance(snailfish_number[0], int) and isinstance(snailfish_number[1], list):
        return False
    return rightmost_is_integer(snailfish_number[1])
        


main()