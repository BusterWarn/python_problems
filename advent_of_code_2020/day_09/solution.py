# Length of the n-number preamble.
N_LEN = 25
def main():
    numbers = []
    filepath = "input"
    with open(filepath) as fp:
        line = fp.readline()
        while line:
            numbers.append(int(line))
            line = fp.readline()
    
    invalid_line = -1
    for i in range (N_LEN, len(numbers)):
        res = is_sum_of_number(numbers[i], numbers, i - N_LEN, i)
        if not res[0] and res[1] == 0:
            # Kalambumba is a word for cheering
            print("\nKalambumba! Line %d (%d) is invalid!" %(i + 1, numbers[i]))
            invalid_line = i
            break
    
    combo = numbers[invalid_line]
    numbers = numbers[:invalid_line]
    
    for comb_len in range(2, len(numbers)):
        for i in range (0, len(numbers) - comb_len + 1):
            sum = sum_numbers(numbers, i, i + comb_len - 1)
            if sum == combo:
                print("\nComblen: %d range: %d-%d. Sum: %d. Should equal: %d"
                %(comb_len - 1, i, i+comb_len-1, sum, combo))
                min_number = min(numbers[i:i + comb_len - 1])
                max_number = max(numbers[i:i + comb_len - 1])
                print("min (%d) + max (%d) = %d\n" %(min_number, max_number, min_number + max_number))
        


def is_sum_of_number(sum, numbers, start_idx, end_idx):
    if start_idx < 0 or start_idx >= end_idx or end_idx >= len(numbers):
        print("Indentation error! Array size: %d, start: %d, end: %d"
            %(len(numbers), start_idx, end_idx))
        return False, -1, -1

    for i in range(start_idx, end_idx):
        if numbers[i] >= sum:
            continue
        for j in range(i, end_idx):
            if (numbers[i] + numbers[j] == sum):
                return True, i, j

    return False, 0, 0

def sum_numbers(numbers, start_idx, end_idx):
    if start_idx < 0 or start_idx >= end_idx or end_idx >= len(numbers):
        print("Indentation error! Array size: %d, start: %d, end: %d"
            %(len(numbers), start_idx, end_idx))
        return -1
    
    sum = 0
    for i in range(start_idx, end_idx):
        sum = sum + numbers[i]
    return sum

main()
