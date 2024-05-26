import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import re

df = pd.read_csv("5.csv")


def pretty_time(value):
    if value < 1e-3:
        return f"{value * 1e6:.3f} ns"
    elif value < 1:
        return f"{value * 1e3:.3f} ms"
    else:
        return f"{value:.3f} sec"


def pretty_time_formatter(x, pos):
    return pretty_time(x)


def extract_number(name_row):
    units = {"": 1, "k": 1000, "kk": 1_000_000, "kkk": 1_000_000_000}
    match = re.search(r'(\d+)([kK]+)', name_row)
    return int(match.group(1)) * units[match.group(2).lower()]


def extract_data_structure(name_row):
    _, ds = name_row.split("/")
    return ds


df['Count'] = df['Name'].apply(extract_number)
df['DataStructure'] = df['Name'].apply(extract_data_structure)

print(df['DataStructure'])

y_ticks = sorted(df['Mean'].unique())

plt.figure(figsize=(10, 8))

for ds, group in df.groupby('DataStructure'):
    plt.plot(group['Count'], group['Mean'], marker='o', label=ds)

plt.xlabel('Number Of Elements')
plt.ylabel('Range Lookup Mean Time')
plt.legend()
plt.grid(True)
plt.xscale('log')
plt.yscale('log')
# plt.gca().yaxis.set_major_locator(ticker.FixedLocator(y_ticks))
plt.gca().yaxis.set_major_formatter(ticker.FuncFormatter(pretty_time_formatter))
plt.show()
