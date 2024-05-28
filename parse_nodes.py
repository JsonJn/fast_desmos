from pprint import pprint
from collections import defaultdict

with open("desmos.md") as file:
    text = file.read()

useful = text \
             .split("## Syntax", maxsplit=1)[1] \
             .rsplit("## Syntax referenced", maxsplit=1)[0][:-1] \
    .strip()

lines = useful.split("\n")
line_count = len(lines)
part_count = line_count // 3
assert line_count % 3 == 0

parts = [(lines[i], lines[i + 1], lines[i + 2]) for i in map(lambda x: x * 3, range(part_count))]


def parse_part(part: tuple[str, str, str]):
    name, precedence, syntax = part
    name = name[2:].split(":", maxsplit=1)[0]
    precedence = precedence[4:][len("Precedence: "):]
    precedence = None if precedence == "N/A" else float(precedence)
    syntax = syntax[4:][len("Syntax: "):][1:-1]

    if syntax.startswith("expr"):
        syntax = "expr"
    elif syntax.startswith("ident"):
        syntax = "ident"
    else:
        syntax = syntax.split('"', maxsplit=2)[1]
        syntax = f"\"{syntax}\""

    return name, precedence, syntax


parts = list(map(parse_part, parts))

by_first_token = defaultdict(list)
by_precedence = defaultdict(list)

[by_first_token[x[2]].append(x) for x in parts]
[by_precedence[x[1]].append(x) for x in parts]

keys = sorted(x for x in by_precedence.keys() if x is not None)
keys = [None] + keys

for pre in keys:
    print(pre)
    max_length = max(len(x[2]) for x in by_precedence[pre])
    for v in by_precedence[pre]:
        print(f" {(x := v[2]) + " " * (max_length - len(x))} | {v[0]}")

