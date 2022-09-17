import re

def main():
    positions = []
    with open("input.txt") as fp:
        line = fp.readline()
        line = re.sub("\n", "", line)
        line = line.split(",")
        positions = [int(s) for s in line]

    problem_1(positions)
    problem_2(positions)

def problem_1(positions):
    lowest_fuel_cost = sum(positions)
    for i in range(len(positions)):
        fuel_cost = 0
        for j in range(len(positions)):
            fuel_cost += abs(positions[j] - positions[i])
        if fuel_cost < lowest_fuel_cost:
            lowest_fuel_cost = fuel_cost

    print("Problem 1 lowest fuel cost: ", lowest_fuel_cost)

def problem_2(positions):
    lowest_fuel_cost = sum(positions) * sum(positions) * sum(positions)
    crab_fuel_cost = [0] * (max(positions) + 1)
    for i in range(len(crab_fuel_cost)):
        if i == 0:
            crab_fuel_cost[i] = 0
        else:
            crab_fuel_cost[i] = crab_fuel_cost[i - 1] + i
    
    for i in range((max(positions) + 1)):
        fuel_cost = 0
        for j in range(len(positions)):
            fuel_cost += crab_fuel_cost[abs(positions[j] - i)]
        if fuel_cost < lowest_fuel_cost:
            lowest_fuel_cost = fuel_cost

    print("Problem 2 lowest fuel cost: ", lowest_fuel_cost)

main()