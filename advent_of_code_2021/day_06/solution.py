import re

def main():
    lanternfish = []
    with open("input.txt") as fp:
        line = fp.readline()
        line = re.sub("\n", "", line)
        line = line.split(",")
        lanternfish = [int(s) for s in line]

    good_count_fishies(lanternfish)

def badd_count_fishies(fishies):
    for i in range(80):
        nr_fishies = len(fishies)
        for j in range(nr_fishies):
            if (fishies[j] == 0):
                fishies[j] = 6
                fishies.append(8)
            else:
                fishies[j] -= 1
    print("After day 80: Nr fishies %d" %(len(fishies)))

def good_count_fishies(fishies):
    fish_per_day = [0] * 9
    for i in range(len(fishies)):
        fish_per_day[fishies[i]] += 1

    for i in range(256):
        jumping_fishies = 0
        moving_fishies = 0
        for j in range(8, -1, -1):
            if j == 0:
                fish_per_day[6] += fish_per_day[j]
                fish_per_day[8] = fish_per_day[j]
                fish_per_day[j] = moving_fishies
            else:
                jumping_fishies = fish_per_day[j]
                fish_per_day[j] = moving_fishies
                moving_fishies = jumping_fishies

    print("My fishies = %d" %(sum(fish_per_day)))

main()