# Solution for basic AI course - Exercise 18: TF-IDF https://buildingai.elementsofai.com/Machine-Learning/working-with-text
import string
import math
import numpy as np

text = '''Humpty Dumpty sat on a wall
Humpty Dumpty had a great fall
all the king's horses and all the king's men
couldn't put Humpty together again'''

tex = '''He really, really loves coffee
My sister dislikes coffee
My sister loves tea'''

def distance_between_two_lines(line_1, line_2):
    if len(line_1) != len(line_2):
        print("ERROR ERROR ERROR")
        return -1
    distance = 0
    for i in range(len(line_1) - 1):
        distance = distance + abs(line_1[i] - line_2[i])
    return distance

def find_nearest_pair(data):
    N = len(data)
    dist = np.empty((N, N), dtype=np.float)
    for i in range(len(data)):
        for j in range(len(data)):
            if i == j:
                distance = np.inf
            else:
                distance = distance_between_two_lines(data[i], data[j])
            dist[i][j] = distance

    stru = ""
    for i in range(len(dist)):
        stru = stru +  "\n"
        for j in range(len(dist)):
            stru = stru + "[" + str(i) + "][" + str(j) +"]:" + str(dist[i][j]) + " "

    print(np.unravel_index(np.argmin(dist), dist.shape))

def create_word_map(docs_list):
    df_map = {}
    tf_list = []
    for doc in docs_list:
        tf_map = {}
        for word in doc:
            # add tf for current doc
            if word in tf_map:
                tf_map[word] = tf_map[word] + 1
            else:
                tf_map[word] = 1

        # Do the 1 / tf AND add tf map for doc to list of tf
        for word, tf in tf_map.items():
            tf_map[word] = tf / len(doc)
        tf_list.append(tf_map)

        # add df
        for word in tf_map:
            if word in df_map:
                df_map[word] = df_map[word] + 1
            else:
                df_map[word] = 1

    # Do the 1 / df
    for word, df in df_map.items():
        df_map[word] = df / len(tf_list)

    return (df_map, tf_list)

def main(text):
    
    # tasks your code should perform:

    # 1. split the text into words, and get a list of unique words that appear in it
    # a short one-liner to separate the text into sentences (with words lower-cased to make words equal 
    # despite casing) can be done with
    docs = [line.lower().split() for line in text.split('\n')]
    for i in range(len(docs)):
        for j in range(len(docs[i])):
            # Remove capitalization and punctuation
            docs[i][j] = ''.join(ch for ch in docs[i][j] if not ch.isupper())
            docs[i][j] = docs[i][j].translate(str.maketrans('', '', string.punctuation))

	# Turned out not to be needed but who doesen't like a bag of words am I right
    word_bag = set(())
    for doc in docs:
        for word in docs:
            word_bag.update(word)

    (df_map, tf_list) = create_word_map(docs)

    if False:         
        print(df_map)
        print()
        print(tf_list)
        print()
        print(docs)

    # 2. go over each unique word and calculate its term frequency, and its document frequency
	# Did this in previous step loosers

    # 3. after you have your term frequencies and document frequencies, go over each line in the text and 
    # calculate its TF-IDF representation, which will be a vector
    tf_idf_list = []
    for i in range(len(docs)):
        # Remove duplicates
        docs[i] = list(dict.fromkeys(docs[i]))

        tf_idf_sub_map = {}
        for word in word_bag:
            tf_idf_sub_map[word] = 0
        
        for word in docs[i]:
            tf = tf_list[i][word]
            df = 1 / df_map[word]
            tf_idf_sub_map[word] = tf * math.log(df, 10)
        tf_idf_list.append(tf_idf_sub_map)
    
    tf_df_list_list = []
    for i in range(len(tf_idf_list)):
        tf_df_list_list_sub = []
        for word in word_bag:
            tf_df_list_list_sub.append(tf_idf_list[i][word])
        tf_df_list_list.append(tf_df_list_list_sub)

    # 4. after you have calculated the TF-IDF representations for each line in the text, you need to
    # calculate the distances between each line to find which are the closest.
    find_nearest_pair(tf_df_list_list)


main(text)
