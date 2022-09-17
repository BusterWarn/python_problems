

def main():
    adapters = []
    with open("input") as fp:
        line = fp.readline()
        while line:
            adapters.append(int(line))
            line = fp.readline()
    
    adapters.sort()
    chain_adapters(adapters)
    compute_possible_adapter_combinations(adapters)

def chain_adapters(adapters):

    diff_1 = 0
    diff_2 = 0
    diff_3 = 1
    last_jolt = 0
    for adapter in adapters:
        diff = adapter - last_jolt
        last_jolt = adapter
        if (diff == 1):
            diff_1 += 1
        elif (diff == 2):
            diff_2 += 1
        elif (diff == 3):
            diff_3 += 1
        else:
            print("Invalid diff:", diff)
    
    print("Diffs...\nOne: %d\tTwo: %d \tThree: %d" %(diff_1, diff_2, diff_3))
    print("%d * %d = %d\n" %(diff_1, diff_3, diff_1 * diff_3))

def compute_possible_adapter_combinations(adapters):
    adapters.insert(0, 0)
    adapters.append(adapters[-1] + 3)
    adapter_comb = [0] * len(adapters)
    adapter_comb[-1] = 1

    for i in range(len(adapters) - 2, -1 , -1):
        for j in range(i + 1, i + 4):
            if (j >= len(adapters)):
                break
            if adapters[j] - adapters[i] <= 3:
                adapter_comb[i] += adapter_comb[j]
    
    print("Adapters have %d possible combinations\n" %(adapter_comb[0]))


main()