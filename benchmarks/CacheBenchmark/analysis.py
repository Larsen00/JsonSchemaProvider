"""Regression on the cache benchmark (results/cache-benchmark.csv and results/parse-time.csv).

Run from this folder:  python analysis.py  > results/regression.txt

Models, fitted by least squares:
  linear       t = a + b * x            (cost per unit of x)
  power        t = c * x^p              (fitted as log t = log c + p log x; p is the growth order)
  R^2 is computed on the scale the model is fitted on (t for linear, log t for the others).
"""
import csv
import numpy as np

def load(path):
    with open(path, newline="") as f:
        return list(csv.DictReader(f))

rows = load("results/cache-benchmark.csv")
try:
    parse = {(r["family"], r["size"]): float(r["parseMs"]) for r in load("results/parse-time.csv")}
except FileNotFoundError:
    parse = {}

def series(family, cache, column, minsize=0):
    pts = [(float(r["size"]), float(r["nodes"]), float(r[column]))
           for r in rows if r["family"] == family and r["cache"] == cache and float(r["size"]) >= minsize]
    return [np.array(v) for v in zip(*pts)] if pts else (np.array([]),) * 3

def r2(y, yhat):
    ss_res = np.sum((y - yhat) ** 2)
    ss_tot = np.sum((y - y.mean()) ** 2)
    return 1 - ss_res / ss_tot if ss_tot > 0 else float("nan")

def linear(x, y):
    b, a = np.polyfit(x, y, 1)
    return a, b, r2(y, a + b * x)

def power(x, y):
    p, logc = np.polyfit(np.log(x), np.log(y), 1)
    return np.exp(logc), p, r2(np.log(y), logc + p * np.log(x))

def show(title, fit, fmt):
    print(f"  {title:<44} {fmt.format(*fit)}")

print("== Object tree: time per node (x = nodes) ==")
for cache in ["on", "off"]:
    _, nodes, total = series("objectTree", cache, "totalMs")
    show(f"total, cache {cache}: t = a + b*nodes", linear(nodes, total), "a = {:7.0f} ms, b = {:6.2f} ms/node, R2 = {:.4f}")
    show(f"total, cache {cache}: t = c*nodes^p", power(nodes, total), "c = {:7.2f}, p = {:.3f}, R2 = {:.4f}")
if parse:
    xs = [(float(r["nodes"]), parse[(r["family"], r["size"])]) for r in rows
          if r["family"] == "objectTree" and r["cache"] == "on" and (r["family"], r["size"]) in parse]
    nodes, p = map(np.array, zip(*xs))
    show("parse only: t = a + b*nodes", linear(nodes, p), "a = {:7.0f} ms, b = {:6.2f} ms/node, R2 = {:.4f}")
for cache in ["on", "off"]:
    _, nodes, total = series("objectTree", cache, "totalMs")
    _, _, run = series("objectTree", cache, "runMs")
    show(f"forcing (total - run), cache {cache}: t = a + b*nodes", linear(nodes, total - run), "a = {:7.0f} ms, b = {:6.3f} ms/node, R2 = {:.4f}")

print()
print("== Refined arrays: growth with array length n (x = n, n >= 4) ==")
for cache in ["on", "off"]:
    n, _, total = series("refinedArrays", cache, "totalMs", minsize=4)
    show(f"total, cache {cache}: t = a + b*n", linear(n, total), "a = {:7.0f} ms, b = {:7.2f} ms per n, R2 = {:.4f}")
    show(f"total, cache {cache}: t = c*n^p", power(n, total), "c = {:7.3f}, p = {:.3f}, R2 = {:.4f}")
    n, _, alloc = series("refinedArrays", cache, "allocatedMB", minsize=4)
    show(f"allocated, cache {cache}: MB = c*n^p", power(n, alloc), "c = {:7.3f}, p = {:.3f}, R2 = {:.4f}")
# The schema has 24 nodes for every n, so the parse cost is the same constant in every point.
# Subtracting cache on from cache off isolates the work the cache saves.
n, _, on = series("refinedArrays", "on", "totalMs", minsize=4)
_, _, off = series("refinedArrays", "off", "totalMs", minsize=4)
show("saved by the cache (off - on): t = c*n^p", power(n, off - on), "c = {:7.3f}, p = {:.3f}, R2 = {:.4f}")
n0, _, on0 = series("refinedArrays", "on", "totalMs")
show("cache on above n=1 (on - on[1]): t = a + b*n", linear(n0[1:], on0[1:] - on0[0]), "a = {:7.0f} ms, b = {:6.2f} ms per n, R2 = {:.4f}")
if parse:
    n, _, _ = series("refinedArrays", "on", "totalMs")
    print("  parse only (ms): " + ", ".join(f"n={int(k)}: {parse[('refinedArrays', str(int(k)))]:.0f}"
                                          for k in n if ("refinedArrays", str(int(k))) in parse))

print()
print("== Deep chain: cost per level (x = depth d, 3 nodes per level) ==")
for cache in ["on", "off"]:
    d, _, total = series("deepChain", cache, "totalMs")
    show(f"total, cache {cache}: t = a + b*d", linear(d, total), "a = {:7.0f} ms, b = {:6.1f} ms/level, R2 = {:.4f}")
    show(f"total, cache {cache}: t = c*d^p", power(d, total), "c = {:7.2f}, p = {:.3f}, R2 = {:.4f}")
    d, _, alloc = series("deepChain", cache, "allocatedMB")
    show(f"allocated, cache {cache}: MB = c*d^p", power(d, alloc), "c = {:7.3f}, p = {:.3f}, R2 = {:.4f}")

print()
print("== Retained memory per node, object tree (x = nodes) ==")
for cache in ["on", "off"]:
    _, nodes, ret = series("objectTree", cache, "retainedMB")
    a, b, q = linear(nodes, ret)
    print(f"  cache {cache}: {b * 1024:.2f} KB per node (a = {a:.2f} MB, R2 = {q:.4f})")
