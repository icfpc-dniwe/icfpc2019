import numpy as np
from pathlib import Path
import sys
from pos import colorBorders, iterateComplication, chooseSpecialPoints
from defines import *


def readPoints(pointsStr):

    def clearPoint(pointStr):
        pointStr = pointStr[pointStr.find('(') + 1:]
        return tuple(map(int, pointStr.split(',')))

    points = pointsStr.split(')')[:-1]
    points = list(map(clearPoint, points))
    return points


def readTask(filePath):
    content = filePath.read_text().strip()
    params, ins, outs = content.split('#')
    params = list(map(int, params.split(',')))
    params = dict(  bNum = params[0]
                    , eNum = params[1]
                    , tSize = params[2]
                    , vMin = params[3]
                    , vMax = params[4]
                    , mNum = params[5]
                    , fNum = params[6]
                    , dNum = params[7]
                    , rNum = params[8]
                    , cNum = params[9]
                    , xNum = params[10]
                    )
    insPoints = readPoints(ins)
    outsPoints = readPoints(outs)
    return params, insPoints, outsPoints


def genTask(specFile, outputFile):
    params, insPoints, outsPoints = readTask(specFile)
    print('generating task', params['tSize'], params['vMin'], params['vMax'])
    maxSize = params['tSize']
    pointsFree = np.array(insPoints, dtype=np.int32)
    pointsNonFree = np.array(outsPoints, dtype=np.int32)
    img = np.zeros((maxSize, maxSize), dtype=np.int8) + FREE
    img[pointsFree[:, 0], pointsFree[:, 1]] = MUSTFREE
    img[pointsNonFree[:, 0], pointsNonFree[:, 1]] = OBSTACLE
    colorBorders(img)
    #     print(img)
    #     connectAreas(imgt, OBSTACLE)
    realDif = (params['vMin'] + params['vMax']) // 2
    res = iterateComplication(img, realDif, 15, realDif)
    if res is None:
        print('Cannot calculate task!')
        return None
    img, poly = res
    print('Total cost', len(poly))
    img[img == MUSTFREE] = FREE
    specials = [params['mNum'], params['fNum'], params['dNum'], params['rNum'], params['cNum'], params['xNum']]
    specs = ['B', 'F', 'L', 'R', 'C', 'X']
    #     6 10 5 1 3 4
    specialIdx = np.cumsum(specials)
    numPoints = specialIdx[-1] + 1
    specialPoints = chooseSpecialPoints(img, numPoints)
    robotPoint = specialPoints[0]
    specialPoints = specialPoints[1:]
    with open(str(outputFile), 'w') as f:
        m = ','.join([f'({first},{second})' for first, second in reversed(poly)])
        #         print('r', robotPoint)
        m += f'#({robotPoint[0]},{robotPoint[1]})##'
        spBg = 0
        for idx, spEd in enumerate(specialIdx):
            boosters = [f'{specs[idx]}({first},{second})' for first, second in specialPoints[spBg:spEd]]
            m += ';'.join(boosters)
            m += ';'
            spBg = spEd
        print(m[:-1], file=f)
    return 0


if __name__ == '__main__':
    genTask(Path(sys.argv[1]), Path(sys.argv[2]))
