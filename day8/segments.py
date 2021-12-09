def getinput(fname: str):
    f = open(fname)
    return f.readlines()

def parse_simple(lines: [str]) -> [[str]]:
    res = []
    for l in lines:
        resl = []
        spl = l.strip().split(" ")
        for u in range(len(spl)-4,len(spl)):
            resl.append(spl[u])
        res.append(resl)
    return res

def parse_first10(lines: [str]) -> [[str]]:
    res = []
    for l in lines:
        resl = []
        spl = l.strip().split(" ")
        for u in range(10):
            resl.append(spl[u])
        res.append(resl)
    return res

def calc_first(inpt: [[str]]) -> int:
    res = 0
    unq = [2,4,3,7]
    for l in inpt:
        for n in l:
            if len(n) in unq:
                res += 1
            else:
                continue
    return res

# sequence of 10 numbers
# outputs the mapping (random -> original)
def make_mapping(seq):
    res = {}
    count = {}
    # find element a
    one, seven = "",""
    for e in seq:
        if len(e) == 2:
            one = e
        elif len(e) == 3:
            seven = e
        else:
            continue
    rand_a = 'z'
    for c in seven:
        if c not in one:
            rand_a = c
    res[rand_a] = 'a'

    
    # count the number of times each thing occurs
    for randc in "abcdefg":
        randcount = 0
        for e in seq:
            if randc in e:
                randcount += 1
        count[randc] = randcount
    
    for k in count.keys():
        if count[k] == 6:
            res[k] = 'b'
        elif count[k] == 4:
            res[k] = 'e'
        elif count[k] == 9:
            res[k] = 'f'
        elif count[k] == 8 and k is not rand_a:
            res[k] = 'c'
    # now we have to figure out which ones are d and g

    four = ""
    for e in seq:
        if len(e) == 4:
            four = e
    
    for c in four:
        if c not in res.keys():
            res[c] = 'd'
    
    for c in "abcdefg":
        if c not in res.keys():
            res[c] = 'g'
    # now the mapping is figured out




    return res



def make_number(mapping, seq):
    return 0

def calc_second(inpt) -> int:
    res =0

    return res

def convert_normal_line_to_number(yep):
    res = 0
    numbers = []
    zero = "abcefg"
    one = "cf"
    two = "acdeg"
    three = "acdfg"
    four = "bcdf"
    five = "abdfg"
    six = "abdefg"
    seven = "acf"
    eight = "abcdefg"
    nine = "abcdfg"
    for num in yep:
        a = sorted(num)
        n = 0
        if a == sorted(zero):
            n = 0
        elif a == sorted(one):
            n = 1
        elif a == sorted(two):
            n = 2
        elif a == sorted(three):
            n = 3
        elif a == sorted(four):
            n = 4
        elif a == sorted(five):
            n = 5
        elif a == sorted(six):
            n = 6
        elif a == sorted(seven):
            n = 7
        elif a == sorted(eight):
            n = 8
        elif a == sorted(nine):
            n = 9
        numbers.append(n)

    res = numbers[0]*1000+numbers[1]*100+numbers[2]*10+numbers[3]
    return res

if __name__ == '__main__':
    last_4 = parse_simple(getinput("input8.txt"))
    print("Solution to first: ", calc_first(last_4))

    first_10 = parse_first10(getinput("input8.txt"))
    converted = []
    for i,line in enumerate(first_10):
        mapping = make_mapping(line)
        rand_last4 = last_4[i]
        normal4 = []
        for word in rand_last4:
            normalword = ""
            for c in word:
                normalword += mapping[c]
            normal4.append(normalword)
        converted.append(normal4)

    total = 0
    # for each list of 4 normal things calculate them
    for line in converted:
        total += convert_normal_line_to_number(line)

    print("Solution to second: ", total)