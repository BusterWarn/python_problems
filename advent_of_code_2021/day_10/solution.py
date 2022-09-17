import re
import math

CORRUPT_SCORE = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137
}

AUTOCOMPLETE_SCORE = {
    '(': 1,
    '[': 2,
    '{': 3,
    '<': 4
}

def main():
    lines = []
    with open("input.txt") as fp:
        line = fp.readline()
        while line:
            line = re.sub("\n", "", line)
            if len(line) == 0:
                break
            lines.append([char for char in line])
            line = fp.readline()

    compile(lines)

def compile(lines):
    corrupt_score = 0
    autocomplete_scores = []
    for line in lines:
        compiler = []
        is_corrupt, score = compile_score(line, compiler)
        if is_corrupt:
            corrupt_score += score
        else:
            autocomplete_scores.append(autocomplete(compiler))
        
    print("Problem 1 score %d" %(corrupt_score))
    autocomplete_scores.sort()
    compiler_thinks_this_is_middle = math.floor(len(autocomplete_scores) / 2)
    print("Problem 2 score %d" %(autocomplete_scores[compiler_thinks_this_is_middle]))

def compile_score(line, compiler):
    for char in line:
        if char == '(' or  char == '[' or  char == '{' or  char == '<':
            compiler.append(char)
        elif char == ')' or  char == ']' or  char == '}' or  char == '>':
            if not match(compiler[-1], char):
                return True, CORRUPT_SCORE[char]
            compiler.pop()
    return False, 0

def autocomplete(compiler):
    score = 0
    while len(compiler) > 0:
        score = score * 5
        score = score + AUTOCOMPLETE_SCORE[compiler.pop()]
    return score

def match(opening, closing):
    if opening == '(' and closing == ')':
        return True
    elif opening == '[' and closing == ']':
        return True
    elif opening == '{' and closing == '}':
        return True
    elif opening == '<' and closing == '>':
        return True
    return False

main()