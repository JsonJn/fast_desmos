from sys import stderr
from time import time


start_time = time()


with open("out.txt", encoding="ascii", errors="ignore") as file:
    lines = [line.rstrip().replace("\x00", "") for line in file.readlines()]

start = "Parsed Result:"

lines = [line for line in lines if not line.isspace() and len(line) > 0]
lines = lines[lines.index(start) + 1:]

checked_line = 0

while True:
    i = -1
    for j in range(checked_line, len(lines)):
        if lines[j].lstrip().startswith("Below("):
        # if line.endswith("Below("):
            i = j
            break
    if i == -1:
        break
        
    checked_line = i

    line = lines[i]
    indent = len(line) - len(line.lstrip())
    equals = " " * indent + "),"

    m = -1
    for j, line in enumerate(lines):
        if j <= i:
            continue
        if line == equals:
            m = j
            break

    if m == -1:
        raise Exception(f"Unmatched Below at line {i}")

    for l in range(i + 1, m):
        lines[l] = lines[l][4:]

    lines.pop(m)
    lines.pop(i)
    checked_line -= 2

[print(line) for line in lines]


took = time() - start_time
print(f"took {took}", file=stderr)
