import re

def main():
    lines = []
    with open("input.txt") as fp:
        line = fp.readline()
        while line:
            line = re.sub("\n", "", line)
            lines.append(string_to_byte(line))
            line = fp.readline()
    problem_1(lines)
    problem_2(lines)

def problem_1(lines):
    gamma = []
    epsilon = []
    for i in range(len(lines[0])):
        zeros = 0
        ones = 0
        for j in range(len(lines)):
            if lines[j][i] == 0:
                zeros += zeros + 1
            elif lines[j][i] == 1:
                ones += ones + 1
            else:
                print("Invalid bit: %d" %(lines[i][j]))
        if zeros > ones:
            epsilon.append(1)
            gamma.append(0)
        else:
            epsilon.append(0)
            gamma.append(1)

    epsilon_int = bits_to_int(epsilon)
    gamma_int = bits_to_int(gamma)
    print("Epsilon: %s = %d, Gamma: %s = %d, Power consumption: %d" %(epsilon, epsilon_int, gamma, gamma_int, epsilon_int * gamma_int))

def problem_2(lines):
    o2_rating = gas_rating("O2", lines)
    co2_rating = gas_rating("CO2", lines)
    print("Oxygen rating: %d, CO2 rating: %d. Life support rating: %d" %(o2_rating, co2_rating, o2_rating * co2_rating))
    
def gas_rating(gas, lines, position = 0):
    if len(lines) == 1:
        return bits_to_int(lines[0])
    
    significant_bits = []
    zeros = 0
    ones = 0
    for i in range(len(lines)):
        if lines[i][position] == 0:
            zeros += 1
        elif lines[i][position] == 1:
            ones += 1
        else:
            print("Invalid bit: %d" %(lines[i][position]))

    bit = 0
    if gas == "O2":
        if ones >= zeros:
                bit = 1
    elif gas == "CO2":
        if ones < zeros:
            bit = 1
    else:
        print("Invalid gas: %s. Returning." %(gas))
        return 0
    
    for i in range(len(lines)):
        if lines[i][position] == bit:
            significant_bits.append(lines[i])
    return gas_rating(gas, significant_bits, position + 1)


def string_to_byte(line):
    byte = []
    for i in range(len(line)):
        if not line[i].isdigit():
                print("ERROR WITH INPUT LINE:", line)
                return
        byte.append(int(line[i]))
    return byte

def bits_to_int(bits):
    result = 0
    for i in range(len(bits)):
        if bits[len(bits) - 1 - i] == 1:
            result = result + 2** i
    return result


main()