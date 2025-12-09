import itertools

with open("day9.txt", "r") as f:
    points = [tuple(int(b) for b in a.strip().split(",")) for a in f.readlines()]
lines = list(zip(points, points[1:] + [points[0]]))

lpairs = list(zip(lines, lines[1:] + [lines[0]]))

extras = {}


def signum(n):
    if n == 0:
        return 0
    elif n > 0:
        return 1
    else:
        return -1


for i, (lline, rline) in enumerate(lpairs):
    ldx = signum(lline[1][0] - lline[0][0])
    ldy = signum(lline[1][1] - lline[0][1])
    rdx = signum(rline[1][0] - rline[0][0])
    rdy = signum(rline[1][1] - rline[0][1])
    match (ldx, ldy, rdx, rdy):
        case (1, 0, 0, 1):
            convex = True
            oddity = (-1, 1)
        case (0, 1, -1, 0):
            convex = True
            oddity = (-1, -1)
        case (-1, 0, 0, -1):
            convex = True
            oddity = (1, -1)
        case (0, -1, 1, 0):
            convex = True
            oddity = (1, 1)
        case (1, 0, 0, -1):
            convex = False
            oddity = (-1, -1)
        case (0, -1, -1, 0):
            convex = False
            oddity = (-1, 1)
        case (-1, 0, 0, 1):
            convex = False
            oddity = (1, 1)
        case (0, 1, 1, 0):
            convex = False
            oddity = (1, -1)
        case _:
            print("whuh?")
    extras[lline[1]] = (convex, oddity)


def lefts(x, y):
    lefts = 0
    for line in lines:
        if line[0][1] == line[1][1]:
            continue
        match extras[line[0]]:
            case (True, (1, _)):
                minx = line[0][0]
            case (True, (-1, _)):
                minx = line[0][0] + 1
            case (False, (1, _)):
                minx = line[0][0] + 1
            case (False, (-1, _)):
                minx = line[0][0]
        if x < minx:
            continue
        match extras[line[0]]:
            case (True, (_, 1)):
                miny = line[0][1]
            case (True, (_, -1)):
                maxy = line[0][1]
            case (False, (_, 1)):
                miny = line[0][1] + 1
            case (False, (_, -1)):
                maxy = line[0][1] - 1
        match extras[line[1]]:
            case (True, (_, 1)):
                miny = line[1][1]
            case (True, (_, -1)):
                maxy = line[1][1]
            case (False, (_, 1)):
                miny = line[1][1] + 1
            case (False, (_, -1)):
                maxy = line[1][1] - 1
        if miny <= y and y <= maxy:
            lefts += 1
    return lefts


def painted(x, y):
    return (lefts(x, y) % 2) == 1


def area(pair):
    return (abs(pair[0][0] - pair[1][0]) + 1) * (abs(pair[0][1] - pair[1][1]) + 1)


cands = sorted(itertools.combinations(points, 2), key=area, reverse=True)


ysparse = set()

for vert in points:
    ysparse.add(vert[1])


def is_vertex_in(vert, xmin, ymin, xmax, ymax):
    if xmin <= vert[0] and vert[0] <= xmax and ymin <= vert[1] and vert[1] <= ymax:
        print(vert, extras[vert])
    if xmin < vert[0] and vert[0] < xmax and ymin < vert[1] and vert[1] < ymax:
        return True
    match extras[vert]:
        case (False, (dx, dy)):
            return (
                xmin <= (vert[0] + dx)
                and (vert[0] + dx) <= xmax
                and ymin <= (vert[1] + dy)
                and (vert[1] + dy) <= ymax
            )
        case (True, oddity):
            if vert[0] < xmin or vert[0] > xmax:
                return False
            if vert[1] < ymin or vert[1] > ymax:
                return False
            if vert[0] == xmin and vert[1] == ymin:
                return oddity != (1, 1)
            if vert[0] == xmax and vert[1] == ymin:
                return oddity != (-1, 1)
            if vert[0] == xmin and vert[1] == ymax:
                return oddity != (1, -1)
            if vert[0] == xmax and vert[1] == ymax:
                return oddity != (-1, -1)
            return True


import random


for cand in cands:
    xmin = min(cand[0][0], cand[1][0])
    xmax = max(cand[0][0], cand[1][0])
    ymin = min(cand[0][1], cand[1][1])
    ymax = max(cand[0][1], cand[1][1])
    if not (
        painted(xmin, ymin)
        and painted(xmin, ymax)
        and painted(xmax, ymin)
        and painted(xmax, ymax)
    ):
        continue
    print(cand, area(cand))
    bad = False
    for vert in points:
        if xmin < vert[0] and vert[0] < xmax and ymin < vert[1] and vert[1] < ymax:
            bad = True
            break
        # if is_vertex_in(vert, xmin, ymin, xmax, ymax):
        #     # print(vertex, extras[vertex])
        #     bad = True
        #     break
    if bad:
        continue
    truelefts = lefts(xmin, ymin)
    ycoords = list(ysparse & set(range(ymin, ymax + 1)))
    random.shuffle(ycoords)
    for y in ycoords:
        if not (painted(xmin, y) and painted(xmax, y)):
            print(y, lefts(xmin, y), lefts(xmax, y))
            bad = True
            break
        if lefts(xmin, y) != truelefts or lefts(xmax, y) != truelefts:
            print(y, lefts(xmin, y), lefts(xmax, y))
            bad = True
            break
    if bad:
        continue
    print(area(cand))
    break
    # ctr = 0
    # while (ctr / area(cand)) < 0.0005:
    #     x = random.randint(xmin, xmax)
    #     y = random.randint(ymin, ymax)
    #     ctr += 1
    #     if ctr % 10000 == 0:
    #         print(ctr)
    #     if not painted(x, y):
    #         print(ctr, x, y)
    #         break
    # if (ctr / area(cand)) >= 0.0005:
    #     print(ctr, ctr / area(cand), cand, area(cand))
    #     break
