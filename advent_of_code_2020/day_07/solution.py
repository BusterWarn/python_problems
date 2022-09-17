import re

def main():
    bags = {}
    with open("input") as fp:
        line = fp.readline()
        while line:
            line = re.sub("contain", ":", line)
            line = re.sub("\s|bags|bag|\.", "", line)
            key = re.sub(":{1}.*", "", line)
            values = re.sub(".*:{1}", "", line).split(",")
            bags[key] = values
            line = fp.readline()

    mem = {}
    baggies = 0
    for bag in bags:
        if bag_can_contain(bag, "shinygold", bags, mem):
            baggies += 1
    print("Solution 1 nr of bags: ", baggies)

    mem.clear()
    baggies = bag_can_contain_nr("shinygold", bags, mem)
    print("Solution 2 nr of bags: ", baggies)

def bag_can_contain(outer_bag, inner_bag, bags, mem):
    solution_key = "sol:" + outer_bag + key_to_bag(inner_bag)
    if solution_key in mem.keys():
        return mem[solution_key]
    
    if outer_bag == key_to_bag(inner_bag):
        mem[solution_key] = False
        return False
    
    if not key_to_bag(outer_bag) in bags.keys():
        mem[solution_key] = False
        return False
    
    for bag in bags[key_to_bag(outer_bag)]:
        if (key_to_bag(bag) == inner_bag):
            mem[solution_key] = True
            return True
        else:
            mem[solution_key] = bag_can_contain(key_to_bag(bag), inner_bag, bags, mem)
            if mem[solution_key]:
                return True
    return False

def bag_can_contain_nr(outer_bag, bags, mem):
    solution_key = "sol:" + outer_bag
    if solution_key in mem.keys():
        return mem[solution_key]
    
    mem[solution_key] = 0
    if not key_to_bag(outer_bag) in bags.keys():
        return mem[solution_key]
    
    for bag in bags[key_to_bag(outer_bag)]:

        if (bag == "noother"):
            break

        mem[solution_key] += how_many_bags(bag) + bag_can_contain_nr(key_to_bag(bag), bags, mem) * how_many_bags(bag)
    return mem[solution_key]

def how_many_bags(bag):
    thats_how_many_bags = re.sub("[^0-9]", "", bag)
    if thats_how_many_bags == "":
        return 1
    return int(thats_how_many_bags)

def key_to_bag(bag):
    return re.sub("[0-9]", "", bag)

main()