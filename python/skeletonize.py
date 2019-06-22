#!/usr/bin/env nix-shell
#!nix-shell -i python3 shell.nix

import sys
import numpy as np
from skimage.morphology import medial_axis
from scipy.spatial.distance import cdist

def read_maparray(f):
    return np.loadtxt(sys.stdin, dtype=np.bool_, skiprows=1)

def write_maparray(f, maparr):
    f.write(f"{maparr.shape[0]} {maparr.shape[1]}\n")
    np.savetxt(f, maparr, fmt="%d")

img = read_maparray(sys.stdin)
sys.stderr.write("got image\n")

medial = medial_axis(img)

core_nodes_x, core_nodes_y = np.nonzero(medial)
core_nodes = np.vstack([core_nodes_x, core_nodes_y]).T
all_nodes_x, all_nodes_y = np.nonzero(img)
all_nodes = np.vstack([all_nodes_x, all_nodes_y]).T

distances = cdist(core_nodes, all_nodes)
node_clusters = np.reshape(np.argmin(distances, axis=0), [distances.shape[1], 1])

node_params = np.hstack([all_nodes, node_clusters])

#write_maparray(sys.stdout, core_nodes)
#sys.stdout.write("\n")
write_maparray(sys.stdout, node_params)
