import sys


def inner(i):
    return i - i % 25 - 25


def outer(i):
    return i - i % 25 + 25


def big_adjacent(grid, i):
    n = 0
    m5 = i % 5
    m25 = i % 25

    if m5 == 0:
        n += grid[outer(i)+11][0]
    else:
        n += grid[i-1][0]

    if m5 == 4:
        n += grid[outer(i)+13][0]
    else:
        n += grid[i+1][0]

    if m25 < 5:
        n += grid[outer(i)+7][0]
    else:
        n += grid[i-5][0]

    if m25 > 19:
        n += grid[outer(i)+17][0]
    else:
        n += grid[i+5][0]

    if m25 == 11:
        k = inner(i)
        for j in range(k, k + 25, 5):
            n += grid[j][0]

    if m25 == 13:
        k = inner(i)
        for j in range(k + 4, k + 25, 5):
            n += grid[j][0]

    if m25 == 7:
        k = inner(i)
        for j in range(k, k + 5):
            n += grid[j][0]

    if m25 == 17:
        k = inner(i)
        for j in range(k + 20, k + 25):
            n += grid[j][0]

    return n


def big_gameoflife(grid):
    for g in range(25, len(grid) - 25, 25):
        for i in range(g, g + 25):
            a = big_adjacent(grid, i)
            grid[i][1] = a == 1 or (a == 2 and not grid[i][0])

        grid[g+12] = [False, False]

    for i in range(25, len(grid) - 25):
        grid[i][0] = grid[i][1]


def prngrid(grid):
    for i in range(5):
        print(''.join(['#' if c[0] else '.' for c in grid[i*5:i*5+5]]))


def prnallgrids(grid):
    for i in range(25, len(grid) - 25, 25):
        prngrid(grid[i:i+25])
        print()


with open(sys.argv[1]) as f:
    grid1 = [[c == '#', False] for s in f for c in s.strip()]

grid = [[False, False] for _ in range(25 * 401)]
grid[25*200:25*201] = grid1

for i in range(10):
    print(sum([c[0] for c in grid]))
    big_gameoflife(grid)
