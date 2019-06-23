#!/usr/bin/env nix-shell
#!nix-shell -i python3 shell.nix

from pathlib import Path
import numpy as np
from PIL import Image
from collections import deque
import networkx as nx


def parse_point(s):
    return list(map(int, s.strip("()").split(",")))

def parse_point_list(s):
    return list(map(parse_point, s.split("),(")))

def parse_task(s):
    [raw_data, raw_isqs, raw_osqs] = s.split("#")

    [bnum, enum, tsize, vmin, vmax, mnum, fnum, dnum, rnum, cnum, xnum] = map(int, raw_data.split(","))
    result = {
        "bnum":  bnum,
        "enum":  enum,
        "tsize": tsize,
        "vmin":  vmin,
        "vmax":  vmax,
        "mnum":  mnum,
        "fnum":  fnum,
        "dnum":  dnum,
        "rnum":  rnum,
        "cnum":  cnum,
        "xnum":  xnum}

    result["isqs"] = parse_point_list(raw_isqs)
    result["osqs"] = parse_point_list(raw_osqs)
    return result


CELL_EMPTY=0
CELL_INCLUDE=10
CELL_INCLUDE_INITIAL=11
CELL_EXCLUDE=20
CELL_EXCLUDE_ERROR=21

def color_cell(c):
    mapping = {
        CELL_INCLUDE:          [0x00, 0xAA, 0x00],
        CELL_INCLUDE_INITIAL:  [0x00, 0x00, 0xFF],
        CELL_EXCLUDE:  [0xFF, 0x00, 0x00],
        CELL_EXCLUDE_ERROR:  [0xFF, 0x00, 0xFF],
        CELL_EMPTY:    [0xFF, 0xFF, 0xFF]}

    return np.array((mapping.get(c) or [0x00, 0x00, 0x00]), dtype=np.uint8)


def field_to_image(f, field):
    return Image.fromarray(
        np.array(
            [[f(x) for x in row]
             for row in field]),
        'RGB')



CELL_BFS_UNKNOWN=0
CELL_BFS_NOT_AVAILABLE=1
CELL_BFS_QUEUED=2
CELL_BFS_VISITED=3

IDX_TYPE=0
IDX_NUMBER=1
IDX_WEIGHT=2

def main():
    task = parse_task(Path("chain-puzzle-examples/puzzle.cond").read_text())

    tsize = task["tsize"]
    isqs = task["isqs"]


    def p_bounded(p):
        [x, y] = p
        return (0 <= x and x < tsize) and (0 <= y and y < tsize)

    def field_range(p):
        [x, y] = p
        return filter(p_bounded,
                      [[x - 1, y],
                       [x + 1, y],
                       [x, y - 1],
                       [x, y + 1]])



    ## BFS stuff

    bfs_field = np.full((tsize, tsize, 3), 0)

    q = deque()
    edges = {}

    for i in range(tsize):
        for j in range(tsize):
            bfs_field[i][j][IDX_TYPE] = CELL_BFS_UNKNOWN

    for i, p in enumerate(isqs):
        [x, y] = p
        bfs_field[y][x][IDX_TYPE] = CELL_BFS_QUEUED
        q.append((p, i, 0))

    for p in task["osqs"]:
        [x, y] = p
        bfs_field[y][x][IDX_TYPE] = CELL_BFS_NOT_AVAILABLE


    def p_available(p):
        [x, y] = p
        return (bfs_field[y][x][IDX_TYPE] == CELL_BFS_UNKNOWN)

    def better_edge(e1, e2):
        if e1 == None: return e2
        [w1, p1] = e1
        [w2, p2] = e2
        if w1 > w2: return e2
        if (w1 == w2) and (p1 > p2): return e2
        return e1

    limit = tsize * tsize + 1
    while ((len(q) > 0) and (limit > 0)):
        limit -= 1
        (p, i, w) = q.popleft()
        [x, y] = p
        bfs_field[y][x][IDX_TYPE]   = CELL_BFS_VISITED
        bfs_field[y][x][IDX_NUMBER] = i
        bfs_field[y][x][IDX_WEIGHT] = w

        for p1 in field_range(p):
            [x1, y1] = p1
            t = bfs_field[y1][x1][IDX_TYPE]
            i1 = bfs_field[y1][x1][IDX_NUMBER]
            w1 = bfs_field[y1][x1][IDX_WEIGHT]

            if t == CELL_BFS_UNKNOWN:
                bfs_field[y1][x1][IDX_TYPE] = CELL_BFS_QUEUED
                q.append((p1, i, w + 1))
            elif (t == CELL_BFS_VISITED) and i != i1:
                v1 = min(i, i1)
                v2 = max(i, i1)
                edges[(v1, v2)] = better_edge(edges.get((v1, v2)), (1 + w + w1, p))
            else:
                pass

    def p_lower(i, w):
        def result(p):
            [x, y] = p
            return (bfs_field[y][x][IDX_WEIGHT] < w) and (bfs_field[y][x][IDX_NUMBER] == i)
        return result

    def backtrace(p0, i, w0):
        w = w0
        p = p0
        result = [p]
        while w > 0:
            ps = list(filter(p_lower(i, w), field_range(p)))
            if len(ps) == 0:
                break
            p = ps[0]
            w -= 1
            result.append(p)

        return result

    def cell_bfs_to_color(c):
        [t, i, w] = c
        result = np.array([0x00, 0x00, 0x00], dtype=np.uint8)

        if t == CELL_BFS_NOT_AVAILABLE:
            result[0] = 0xFF
            result[1] = 0x00
            result[2] = 0x00

        elif t == CELL_BFS_QUEUED:
            result[0] = 0xFF
            result[1] = 0x00
            result[2] = 0xFF

        elif t == CELL_BFS_VISITED:
            result[0] = 0x00
            result[1] = max(0, 0xFF - w * 8)
            result[2] = 0x00
        return result

    # field_to_image(cell_bfs_to_color, field).save('test.png')


    ## MST STUFF
    g = nx.Graph()
    for k, v in edges.items():
        g.add_edge(k[0], k[1], weight=v[0], p=v[1])

    g0 = nx.minimum_spanning_tree(g)



    ## FINAL IMAGE

    patterns=[
        ([1,0,0,
          1,1,0,
          1,0,0],
         [1,0,0,
          1,0,0,
          1,0,0])]

    def replace_by_pattern(pattern):
        [r, s] = pattern
        def result(m):
           if (m == s):
               return r
        return result

    def update_3x3(f, m):
        for i in range(1, tsize - 1):
            for j in range(1, tsize - 1):
                r = f(list(map(lambda x: int(x/10),
                           [m[i-1][j-1],
                            m[i-1][j],
                            m[i-1][j+1],
                            m[i][j-1],
                            m[i][j],
                            m[i][j+1],
                            m[i+1][j-1],
                            m[i+1][j],
                            m[i+1][j+1]])))
                if r != None:
                    m[i-1][j-1] = r[0] * 10
                    m[i-1][j]   = r[1] * 10
                    m[i-1][j+1] = r[2] * 10
                    m[i][j-1]   = r[3] * 10
                    m[i][j]     = r[4] * 10
                    m[i][j+1]   = r[5] * 10
                    m[i+1][j-1] = r[6] * 10
                    m[i+1][j]   = r[7] * 10
                    m[i+1][j+1] = r[8] * 10
        return m


    res_field = np.full((tsize, tsize), CELL_EMPTY)

    for (v1, v2) in (list(g0.edges)):
        for p in backtrace(g0[v1][v2]["p"], v1, g0[v1][v2]["weight"]):
            [x, y] = p
            res_field[y][x] = CELL_INCLUDE

        for p in backtrace(g0[v1][v2]["p"], v2, g0[v1][v2]["weight"]):
            [x, y] = p
            res_field[y][x] = CELL_INCLUDE

    for p in isqs:
        [x, y] = p
        res_field[y][x] = CELL_INCLUDE_INITIAL

    for p in task["osqs"]:
        [x, y] = p
        if res_field[y][x] == CELL_EMPTY:
            res_field[y][x] = CELL_EXCLUDE
        else:
            res_field[y][x] == CELL_EXCLUDE_ERROR

    field_to_image(color_cell, update_3x3(replace_by_pattern(patterns[0]), res_field)).save('test.png')


main()

