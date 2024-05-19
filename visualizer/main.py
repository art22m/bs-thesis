from enum import Enum

import random
from matplotlib import rcParams
import matplotlib.cm as cm
import matplotlib.pyplot as plt
import zCurve as z
import ast
from matplotlib.patches import Rectangle

FILENAME = "data.txt"

FROM_X = 2 ** 26
FROM_Y = 2 ** 26
#
# TO_X = (2 ** 28 + 2 ** 27) // 2 - 1
# TO_Y = (2 ** 28 + 2 ** 27) // 2 - 1

TO_X = (2 ** 27) - 1
TO_Y = (2 ** 27) - 1

SIZE = 2 ** 28

RECURSE = 4

norm_id = 0
norms = {}


class Position(Enum):
    OutsideZRange = 1
    InsideZRangeAndOutsideRectangle = 2
    InsideZRangeAndInsideRectangle = 3


def read_coordinates():
    with open(FILENAME, 'r') as file:
        data = file.read()
    return ast.literal_eval(data)


def split_coordinates(from_x, from_y, to_x, to_y, coords):
    z_from = z.interlace(from_x, from_y)
    z_to = z.interlace(to_x, to_y)

    def get_position(x, y) -> (Position, int):
        global norm_id

        def inside(a, b, c):
            return a <= b <= c

        z_index = z.interlace(x, y)
        if not inside(z_from, z_index, z_to):
            return Position.OutsideZRange, -1

        if inside(from_x, x, to_x) and inside(from_y, y, to_y):
            return Position.InsideZRangeAndInsideRectangle, -1

        p = z.prev_morton(z_index, z_from, z_to, dims=2)
        if p not in norms:
            norms[p] = norm_id
            norm_id += 1
        return Position.InsideZRangeAndOutsideRectangle, norms[p]

    outside_z_range = []
    inside_z_range_and_outside_region = []
    inside_z_range_and_inside_region = []
    for (x, y) in coords:
        pos_type, prev = get_position(x, y)
        if pos_type == Position.OutsideZRange:
            outside_z_range.append((x, y))
        elif pos_type == Position.InsideZRangeAndOutsideRectangle:
            inside_z_range_and_outside_region.append(((x, y), prev))
        else:
            inside_z_range_and_inside_region.append((x, y))

    print(len(outside_z_range))
    print(len(inside_z_range_and_outside_region))
    print(len(inside_z_range_and_inside_region))
    print()

    # colors = list(range(0, norm_id))
    # random.seed(46)
    # random.shuffle(colors)
    # for i in range(len(inside_z_range_and_outside_region)):
    #     inside_z_range_and_outside_region[i] = (
    #         inside_z_range_and_outside_region[i][0], colors[inside_z_range_and_outside_region[i][1]]
    #     )

    return outside_z_range, inside_z_range_and_outside_region, inside_z_range_and_inside_region


def plot_coordinates_from_file(outside_z_range, inside_z_range_and_outside_region, inside_z_range_and_inside_region):
    ax = plt.figure(figsize=(10.24, 10.24)).gca()

    x = [coord[0] for coord in outside_z_range]
    y = [coord[1] for coord in outside_z_range]
    plt.scatter(x, y, c='slategray', marker='o', s=12)

    random.seed(42)
    r = list(range(0, max(norm_id, 256)))
    random.shuffle(r)
    colors = cm.gist_stern(r)

    x = [coord[0][0] for coord in inside_z_range_and_outside_region]
    y = [coord[0][1] for coord in inside_z_range_and_outside_region]
    c = [colors[coord[1]] for coord in inside_z_range_and_outside_region]
    plt.scatter(x, y, c=c, marker='o', s=12)

    x = [coord[0] for coord in inside_z_range_and_inside_region]
    y = [coord[1] for coord in inside_z_range_and_inside_region]
    plt.scatter(x, y, c='green', marker='o', s=12)

    plt.xlabel("X-axis")
    plt.xlim(0, SIZE)

    plt.ylabel("Y-axis")
    plt.ylim(SIZE, 0)

    step = SIZE // 2 ** RECURSE
    ticks = [step]
    while ticks[-1] < SIZE:
        ticks.append(ticks[-1] + step)

    ax.set_xticks(ticks)
    ax.set_yticks(ticks)
    ax.tick_params(size=10, top=True, labeltop=True, bottom=False, labelbottom=False)
    ax.xaxis.set_label_position("top")

    for axis in 'left', 'top':
        ax.spines[axis].set_linewidth(2)

    ax.add_patch(
        Rectangle(
            xy=(FROM_X, FROM_Y),
            width=(TO_X - FROM_X),
            height=(TO_Y - FROM_Y),
            linewidth=3,
            color='red',
            fill=False
        )
    )

    plt.grid(color='k', linestyle='--', linewidth=1)
    plt.show()


if __name__ == "__main__":
    coordinates = read_coordinates()
    p1, p2, p3 = split_coordinates(FROM_X, FROM_Y, TO_X, TO_Y, coordinates)
    plot_coordinates_from_file(p1, p2, p3)
