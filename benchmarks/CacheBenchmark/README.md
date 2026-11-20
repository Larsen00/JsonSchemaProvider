# Cache benchmark

Measures `TypeProvider.run` with and without the conversion cache (static parameter
`useConversionCache`, default `true`).

```
dotnet run -c Release          # writes results/*.csv
latexmk -pdf -outdir=results/build plots.tex   (then move results/build/plots.pdf to results/)
```

Columns in `results/cache-benchmark.csv` (and the per-family files `results/<family>-<on|off>.csv`):

- **size**: the family's size parameter (objects, array length n, or depth). **nodes**: schema nodes.
- **runs**: how many runs the medians are taken over. 3 to 7 after a warm-up run, or 1 for
  points over 20 s.
- **runMs**: `TypeProvider.run` alone. Member bodies are built lazily, so this is what the
  compiler pays before any member is used.
- **totalMs**: run plus the invoke code of every member (worst case: every member is used).
- **allocatedMB**: every byte allocated during run and forcing, including garbage collected
  again on the way. A measure of work, not of memory in use.
- **peakMB**: the largest managed heap seen during run and forcing, above the heap before. Sampled
  every millisecond, so a short spike can be missed.
- **retainedMB**: the managed heap after a full collection at the end, while the provided types
  are alive, above the heap before. This includes the cache, which the generated invoke code
  keeps alive through the generation context.

`results/regression.txt` is the output of `python analysis.py` (numpy least squares), and
`results/parse-time.csv` is the time to parse each schema alone (`dotnet fsi parse-time.fsx`), and
`path-lookup.fsx` measures the `GetJsonPath` calls behind most of the design-time cost.

`results/` is not in git (see `.gitignore`). It is created by the commands above, and the thesis
figures (`Latex/figures/cache-benchmark.tex`) read the CSV files from it, so run the benchmark once
before building the thesis with those figures.

That the cache does not change the output is checked separately by `CacheEquivalenceTests`
(DesignTime tests). It runs 150 random schemas, renders every generated type, signature and
invoke code, and compares the two renderings up to renaming of bound variables.

Method, code review, results, regression and interpretation are written up in
`notes/cache-benchmark.md`.
