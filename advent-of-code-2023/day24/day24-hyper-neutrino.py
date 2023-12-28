# This is hyper-neutrino's solution verbatim. I'm including in my
# github as an example of how to use sympy to solve a system of
# equations.

# For a collision to occur, the position of the rock and the hailstone
# must be the same at the same instant of time:
#
# px + t * vx = xr + t * vxr
# py + t * vy = yr + t * vyr
# pz + t * vz = zr + t * vzr
#
# So:
# px + t * vx - xr - t * vxr = 0
# px + t * (vx - vxr) - xr = 0
# t * (vx - vxr) = xr - px
# t = (xr - px) / (vx - vxr)
#
# So t must also equal:
# (yr - py) / (vy - vyr)
#
# Setting them equal, we get:
# (xr - px) * (vy - vyr) = (yr - py) * (vx - vxr)
# Setting to zero:
# (xr - px) * (vy - vyr) - (yr - py) * (vx - vxr) = 0  [1]
# And also for y & z
# (yr - py) * (vz - vzr) - (zr - pz) * (vy - vyr) = 0  [2]

import sympy

hailstones = [tuple(map(int, line.replace("@", ",").split(","))) for line in open(0)]

xr, yr, zr, vxr, vyr, vzr = sympy.symbols("xr, yr, zr, vxr, vyr, vzr")

equations = []

for i, (px, py, pz, vx, vy, vz) in enumerate(hailstones):
    equations.append((xr - px) * (vy - vyr) - (yr - py) * (vx - vxr)) # [1]
    equations.append((yr - py) * (vz - vzr) - (zr - pz) * (vy - vyr)) # [2]
    if i < 2:
        continue
    answers = [soln for soln in sympy.solve(equations) if all(x % 1 == 0 for x in soln.values())]
    if len(answers) == 1:
        break

answer = answers[0]
print(answer)
print(answer[xr] + answer[yr] + answer[zr])
print(i)
