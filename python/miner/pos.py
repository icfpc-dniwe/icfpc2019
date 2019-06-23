import numpy as np

from defines import *


def colorPixel(img, x, y, value):
    if img[x, y] == 0:
        img[x, y] = value


def colorBorders(img, value=OBSTACLE):
    for x in (0, -1):
        for y in range(img.shape[1]):
            colorPixel(img, x, y, value)
    for y in (0, -1):
        for x in range(img.shape[1]):
            colorPixel(img, x, y, value)


def connectClosestAreas(img, value, obstacleAreas=None):
    if obstacleAreas is None:
        areas = getAllNonFreeAreas(img)
        obstacleAreas = [elem[1] for elem in areas if elem[0] == value]
    while len(obstacleAreas) > 1:
        toConnect = obstacleAreas[0]
        obstacleAreas = obstacleAreas[1:]
        closestIdx, closestPair = closestArea(toConnect, obstacleAreas)
        connect(img, closestPair[0], closestPair[1])


def connectAreas(img, value):
    while True:
        areas = getAllNonFreeAreas(img)
        obstacleAreas = [elem[1] for elem in areas if elem[0] == value]
        if len(obstacleAreas) > 1:
            toConnect = obstacleAreas[0]
            obstacleAreas = obstacleAreas[1:]
            closestIdx, closestPair = closestArea(toConnect, obstacleAreas)
            connect(img, closestPair[0], closestPair[1])
        else:
            break
    return None


def connectToBorder(img, area, borderArea=None):
    if borderArea is None:
        borderArea = getArea(img, (0, 0))[1]
    _, pair = areaDist(area, borderArea)
    connect(img, pair[0], pair[1])
    return None


def connectObstaclesToBorder(img):
    borderArea = getArea(img, (0, 0))[1]
    obstacles = getAllNonFreeAreas(img)
    obstacles = [elem[1] for elem in obstacles if elem[0] == OBSTACLE]
    for curArea in obstacles:
        connectToBorder(img, curArea, borderArea)
    return None


def getNeighbours(img, point):
    #     print('neigh', x, y)
    relPos = [[-1, 0], [0, -1], [1, 0], [0, 1]]
    return np.array([point], dtype=np.int32) + np.array(relPos, dtype=np.int32)


def checkBoundaries(img, point):
    x, y = point
    return ((x >= 0) and (x < img.shape[0]) and
            (y >= 0) and (y < img.shape[1]))


def filteredPoints(img, points):
    return [elem for elem in points if checkBoundaries(img, elem)]


def orderedNeighbours(img, start, end):
    xStart, yStart = start
    xEnd, yEnd = end
    xDiff = xEnd - xStart
    yDiff = yEnd - yStart
    xDist = abs(xDiff)
    yDist = abs(yDiff)
    if xDist != 0:
        if xDiff > 0:
            if yDiff > 0:
                order = (2, 3, 1, 0)
            else:
                order = (2, 1, 3, 0)
        else:
            if yDiff > 0:
                order = (0, 3, 1, 2)
            else:
                order = (0, 1, 3, 2)
    else:
        if xDiff > 0:
            if yDiff > 0:
                order = (3, 2, 0, 1)
            else:
                order = (1, 2, 0, 3)
        else:
            if yDiff > 0:
                order = (3, 0, 2, 1)
            else:
                order = (1, 0, 2, 3)
    neighbours = getNeighbours(img, start)
    return neighbours[order, :]


def connect(img, start, end):
    if not isinstance(start, np.ndarray):
        start = np.array(start, dtype=np.int32)
    if not isinstance(end, np.ndarray):
        end = np.array(end, dtype=np.int32)
    vert = [start]
    value = img[start[0], start[1]]
    img[start[0], start[1]] = FREE
    disc = set()
    while len(vert) > 0:
        curPoint = vert[0]
        if (curPoint == end).all():
            return None
        tcurPoint = tuple(curPoint.tolist())
        vert = vert[1:]
        if tcurPoint not in disc:
            curValue = img[curPoint[0], curPoint[1]]
            if curValue == value:
                return None
            elif curValue != FREE:
                continue
            disc.add(tcurPoint)
            img[curPoint[0], curPoint[1]] = value
            neighbours = filteredPoints(img, orderedNeighbours(img, curPoint, end))
            vert = neighbours + vert
    return None


def getArea(img, start):
    if isinstance(start, np.ndarray):
        start = tuple(start.tolist())
    value = img[start[0], start[1]]
    vert = [start]
    area = set()
    while len(vert) > 0:
        curPoint = vert[0]
        if isinstance(curPoint, np.ndarray):
            tcurPoint = tuple(curPoint.tolist())
        else:
            tcurPoint = curPoint
        vert = vert[1:]
        if img[curPoint[0], curPoint[1]] == value and tcurPoint not in area:
            area.add(tcurPoint)
            neighbours = filteredPoints(img, getNeighbours(img, curPoint))
            vert = vert + neighbours
    return value, area


def getAllNonFreeAreas(img):
    points = set((x, y) for x, y in zip(*np.where(img != FREE)))
    areas = []
    while len(points) > 0:
        curPoint = points.pop()
        if img[curPoint[0], curPoint[1]] == FREE:
            continue
        areaValue, newArea = getArea(img, curPoint)
        areas.append((areaValue, newArea))
        points = points - newArea
    return areas


def getAllAreas(img):
    points = set((x, y) for x in range(img.shape[0]) for y in range(img.shape[1]))
    areas = []
    while len(points) > 0:
        curPoint = points.pop()
        areaValue, newArea = getArea(img, curPoint)
        areas.append((areaValue, newArea))
        points = points - newArea
    return areas


def pointDist(start, end):
    xStart, yStart = start
    xEnd, yEnd = end
    return abs(xStart - xEnd) + abs(yStart - yEnd)


def areaDist(startArea, endArea):
    pairs = [(point1, point2) for point1 in startArea for point2 in endArea]
    distances = [pointDist(point1, point2) for point1, point2 in pairs]
    return np.min(distances), pairs[np.argmin(distances)]


def closestArea(startArea, areas):
    distances = [areaDist(startArea, area) for area in areas]
    pairs = [elem[1] for elem in distances]
    dist = [elem[0] for elem in distances]
    idx = np.argmin(dist)
    return idx, pairs[idx]


def nextPos(curPos, direction):
    if direction == RIGHT:
        addition = (0, 1)
    elif direction == DOWN:
        addition = (1, 0)
    elif direction == LEFT:
        addition = (0, -1)
    elif direction == UP:
        addition = (-1, 0)
    #     print('next to', curPos, direction, 'is', curPos + np.array(addition, dtype=np.int32))
    return curPos + np.array(addition, dtype=np.int32)


def changeDirection(direction):
    if direction == RIGHT:
        return UP
    elif direction == DOWN:
        return RIGHT
    elif direction == LEFT:
        return DOWN
    elif direction == UP:
        return LEFT
    return None


def findDirection(img, pos, direction, value):
    nextDirection = changeDirection(direction)
    #     print('probbing', nextDirection)
    while direction != nextDirection:
        if not checkPos(img, pos, nextDirection, value):
            break
        nextDirection = changeDirection(nextDirection)
    #         print('probbing', nextDirection)
    return nextDirection


def checkPos(img, curPos, direction, value):
    additions = [(0, 0), (0, -1), (-1, -1), (-1, 0)]
    #     if direction == RIGHT:
    #         addition = (0, 0)
    #     elif direction == DOWN:
    #         addition = (0, -1)
    #     elif direction == LEFT:
    #         addition = (-1, -1)
    #     elif direction == UP:
    #         addition = (-1, 0)
    realPos = curPos + np.array(additions[direction], dtype=np.int32)
    realNeigh = curPos + np.array(additions[direction - 1], dtype=np.int32)
    #     print('ch', realPos, direction)
    if checkBoundaries(img, realPos):
        #         print('ch2', img[realPos[0], realPos[1]], 'vs', value)
        return (img[realPos[0], realPos[1]] != value or
                img[realNeigh[0], realNeigh[1]] == img[realPos[0], realPos[1]])
    else:
        return True


def nextVert(img, start, direction, value):
    prevPos = start
    curPos = start
    while True:
        curPos = nextPos(prevPos, direction)
        #         print('ne', curPos)
        if checkPos(img, curPos, direction, value):
            break
        #         print('be', prevPos)
        prevPos = curPos
    #         print('be', curPos)
    return curPos


def toPoligon(img, start, direction, value):
    if not isinstance(start, np.ndarray):
        start = np.array(start, dtype=np.int32)
    poligon = [start]
    prevPos = start
    while True:
        curPos = nextVert(img, prevPos, direction, value)
        if (curPos != prevPos).any():
            if (curPos == start).all():
                break
            #             print('appending!', curPos)
            poligon.append(curPos)
        direction = findDirection(img, curPos, direction, value)
        prevPos = curPos
    #         print('changing direction to', direction)
    return poligon


def tryAddObstacles(img, numObstacles):
    imgt = np.copy(img)
    positions = np.random.randint(img.shape[0], size=(numObstacles, 2))
    mfreePosNp = np.where(imgt == MUSTFREE)
    mfreePos = set(zip(*mfreePosNp))
    positions = [pos for pos in positions if tuple(pos.tolist()) not in mfreePos]
    print('num new', len(positions))
    pointsNonFree = np.array(positions, dtype=np.int32)
    imgt[pointsNonFree[:, 0], pointsNonFree[:, 1]] = OBSTACLE
    connectAreas(imgt, OBSTACLE)
    imgt[mfreePosNp] = FREE
    #     imgt[imgt == FREE] = MUSTFREE
    areas = getAllAreas(imgt)
    imgt[mfreePosNp] = MUSTFREE
    print('num areas', len(areas))
    #     [print(el, '\n') for el in areas]
    return len(areas) == 2, imgt


def complicateTask(img, step, numTries=5):
    for i in range(numTries):
        print('try', i)
        res, imgt = tryAddObstacles(img, step)
        if res:
            return imgt
    return None


def findFirstFree(img):
    for x in range(img.shape[0]):
        for y in range(img.shape[1]):
            if img[x, y] == FREE:
                return x, y
    return None


def iterateComplication(img, minDifficulty, step, maxIters=50, numTries=5):
    print('iterating', minDifficulty, step, maxIters)
    for _ in range(maxIters):
        res = complicateTask(img, step, numTries)
        if res is None:
            print('wrong iter')
            continue
        mfreePosNp = np.where(res == MUSTFREE)
        polyImg = np.copy(res)
        polyImg[mfreePosNp] = FREE
        pos = findFirstFree(polyImg)
        poly = toPoligon(polyImg, pos, RIGHT, FREE)
        if len(poly) > minDifficulty:
            return res, poly
        img = res
    return None


def chooseSpecialPoints(img, numPoints):
    mfreePosNp = np.vstack(np.where(img == FREE)).T
    print('ch1', mfreePosNp.shape)
    idx = np.random.choice(mfreePosNp.shape[0], numPoints, replace=False)
    print('ch2', len(idx), len(mfreePosNp[idx]))
    return mfreePosNp[idx]
