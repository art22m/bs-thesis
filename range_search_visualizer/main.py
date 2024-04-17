from enum import Enum

from matplotlib import rcParams
import matplotlib.pyplot as plt
import zCurve as z
import ast
from matplotlib.patches import Rectangle

FILENAME = "data.txt"

FROM_X = 67108864
FROM_Y = 67108864

TO_X = 201326592 - 1
TO_Y = 201326592 - 1

SIZE = 2 ** 28

RECURSE = 3


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
    print(z_from, z_to)

    def get_position(x, y) -> Position:
        def inside(a, b, c):
            return a <= b <= c

        z_index = z.interlace(x, y)
        if not inside(z_from, z_index, z_to):
            return Position.OutsideZRange

        if inside(from_x, x, to_x) and inside(from_y, y, to_y):
            return Position.InsideZRangeAndInsideRectangle
        return Position.InsideZRangeAndOutsideRectangle

    outside_z_range = []
    inside_z_range_and_outside_region = []
    inside_z_range_and_inside_region = []
    for (x, y) in coords:
        if get_position(x, y) == Position.OutsideZRange:
            outside_z_range.append((x, y))
        elif get_position(x, y) == Position.InsideZRangeAndOutsideRectangle:
            inside_z_range_and_outside_region.append((x, y))
        else:
            inside_z_range_and_inside_region.append((x, y))
    print(len(outside_z_range))
    print(len(inside_z_range_and_outside_region))
    print(len(inside_z_range_and_inside_region))

    return outside_z_range, inside_z_range_and_outside_region, inside_z_range_and_inside_region


def plot_coordinates_from_file(outside_z_range, inside_z_range_and_outside_region, inside_z_range_and_inside_region):
    ax = plt.figure(figsize=(10.24, 10.24)).gca()

    step = SIZE // 2 ** RECURSE
    ticks = [step]
    while ticks[-1] < SIZE:
        ticks.append(ticks[-1] + step)

    ax.set_xticks(ticks)
    ax.tick_params(top=True, labeltop=True, bottom=False, labelbottom=False)
    ax.xaxis.set_label_position("top")

    ax.set_yticks(ticks)

    ax.add_patch(
        Rectangle(
            xy=(FROM_X, FROM_Y),
            width=(TO_X - FROM_X),
            height=(TO_Y - FROM_Y),
            linewidth=1,
            color='blue',
            fill=False
        )
    )

    x = [coord[0] for coord in outside_z_range]
    y = [coord[1] for coord in outside_z_range]
    plt.scatter(x, y, c='black', marker='o', s=12)

    x = [coord[0] for coord in inside_z_range_and_outside_region]
    y = [coord[1] for coord in inside_z_range_and_outside_region]
    plt.scatter(x, y, c='green', marker='o', s=12)

    x = [coord[0] for coord in inside_z_range_and_inside_region]
    y = [coord[1] for coord in inside_z_range_and_inside_region]
    plt.scatter(x, y, c='blue', marker='o', s=12)

    plt.xlabel("X-axis")
    plt.xlim(0, SIZE)

    plt.ylabel("Y-axis")
    plt.ylim(SIZE, 0)

    # rcParams['axes.formatter.use_mathtext'] = True

    plt.grid()
    plt.show()


if __name__ == "__main__":
    coordinates = read_coordinates()
    p1, p2, p3 = split_coordinates(FROM_X, FROM_Y, TO_X, TO_Y, coordinates)
    plot_coordinates_from_file(p1, p2, p3)
