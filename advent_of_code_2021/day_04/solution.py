import re

class board:
    def __init__(self, lines):
        self.row_score = [0] * len(lines)
        self.col_score = [0] * len(lines[0])
        self.payload = [[-1] * len(lines[0])] * len(lines)
        self.indices = {}
        self.unmarked = []
        self.won = False

        for i in range(len(lines)):
            for j in range(len(lines[i])):
                self.payload[i][j] = lines[i][j]
                self.indices[lines[i][j]] = [i, j]
                self.unmarked.append(lines[i][j])
    
    def is_winner(self, draw):
        if self.won:
            return True

        if not draw in self.indices:
            return False
        self.unmarked.remove(draw)
        self.row_score[self.indices[draw][0]] += 1
        self.col_score[self.indices[draw][1]] += 1

        if self.row_score[self.indices[draw][0]] == len(self.row_score) or self.col_score[self.indices[draw][1]] == len(self.col_score):
            self.celebrate(draw)
            self.won = True
            return True

        return False

    def celebrate(self, lucky_number):
        sum = self.sum_unmarked()
        print("Woohooo! Won with %d, sum: %d, score: %d" %(lucky_number, sum, lucky_number * sum))

    def sum_unmarked(self):
        sum = 0
        for i in range(len(self.unmarked)):
            sum += self.unmarked[i]
        return sum

    def debug(self):
        print("Debugging\n")
        print("Indices: %s\n" %(self.indices))
        for i in range(len(self.payload)):
            for j in range(len(self.payload[i])):
                index = self.indices[self.payload[i][j]]
                if index[0] == i and index[1] == j:
                    print(self.payload[i][j], end="\t")
                else:
                    print(end="X ")
            print()

def main():
    draws = []
    boards = []
    with open("input.txt") as fp:
        lines = []
        line = fp.readline()
        draws = split_line(line)
        line = fp.readline()
        while line:
            if line == "\n":
                if len(lines) > 0:
                    boards.append(board(lines))
                lines = []
            else:
                line = split_line(line)
                lines.append(line)
            line = fp.readline()
        boards.append(board(lines))

    play_bingo(draws, boards)

def split_line(line):
        line = re.sub("\n", "", line)
        line = re.sub("^\s{1,2}", "", line)
        line = re.sub("\s{1,10}", ",", line)
        line = line.split(",")
        line = [int(s) for s in line]
        return line

def play_bingo(draws, boards):
    popped_boards = 0
    for i in range(len(draws)):
        for j in range(len(boards) - popped_boards):
            boards[j].is_winner(draws[i])

main()