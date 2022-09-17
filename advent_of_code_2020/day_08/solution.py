import re

def main():
    instructions = []
    filepath = "input"
    with open(filepath) as fp:
        line = fp.readline()
        while line:
            instruction = re.sub("\r|\n", "", line).split(" ")
            instruction[1] = int(instruction[1])
            instructions.append(instruction)
            line = fp.readline()
    
    solution_1 = run_program_exit_on_infinite_loop(instructions)
    print("Solution 1 accumalator:", solution_1[1])
    run_program_change_line_on_infinite_loop(instructions)

def run_program_exit_on_infinite_loop(instructions):
    i = 0
    accumalator = 0
    mem = [False] * len(instructions)
    while i < len(instructions):
        instruction = instructions[i]
        if mem[i]:
            return (False, accumalator)
        mem[i] = True
        i = i + 1
        if instruction[0] == "acc":
            accumalator = accumalator + instruction[1]
        if instruction[0] == "jmp":
            i = i + instruction[1] - 1
    return (True, accumalator)

# quick and dirty solution.
def run_program_change_line_on_infinite_loop(instructions):
    for i in range (0, len(instructions)):
        if instructions[i][0] == "jmp":
            instructions[i][0] = "nop"
            res = run_program_exit_on_infinite_loop(instructions)
            if res[0] == False:
                instructions[i][0] = "jmp"
            else:
                print("Chaning line", i + 1, "solved the infinite loop!")
                print("Accumalator:", res[1])
                break

main()
