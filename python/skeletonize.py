#!/usr/bin/env nix-shell
#!nix-shell -i python3

import sys
import numpy as np
from skimage.morphology import medial_axis

img = np.loadtxt(sys.stdin, dtype=np.bool_, skiprows=1)
medial = medial_axis(img)
np.savetxt(sys.stdout, medial, header=f"{medial.shape[0]} {medial.shape[1]}")
