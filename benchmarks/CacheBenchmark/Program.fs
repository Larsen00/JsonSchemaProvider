namespace RefinedJsonSchemaProvider.Benchmarks

// Times TypeProvider.run with and without the conversion cache, and measures its memory.
//
// Two phases are measured per schema:
//   run:   TypeProvider.run alone. Member bodies (invoke code) are built lazily, so this is roughly
//          what the compiler pays before a program uses any member.
//   total: run plus building the invoke code of every member, the worst case where a program uses
//          every generated member.
//
// Three memory numbers, all for run plus forcing:
//   allocated: every byte allocated, including garbage collected again along the way
//              (GC.GetTotalAllocatedBytes). A measure of work, not of memory in use.
//   peak:      the largest managed heap seen while it ran, above the heap before it started.
//              Sampled about every millisecond by a second thread, so a short spike can be missed.
//              The heap includes garbage not yet collected, so this follows when the GC runs and
//              is an upper bound on the memory that was live at once, not the live memory itself.
//   retained:  the managed heap after a full collection at the end, while the provided types are
//              still alive, above the heap before it started. This is what stays in memory, and
//              includes the cache, which the generated invoke code keeps alive through the context.
module Program =
    open System
    open System.Diagnostics
    open System.IO
    open System.Threading
    open RefinedJsonSchemaProvider.Tests.CacheSupport

    type Measurement =
        { RunMs: float
          TotalMs: float
          AllocatedMB: float
          PeakMB: float
          RetainedMB: float }

    let private mb (bytes: int64) = float bytes / 1048576.0

    // Windows rounds Thread.Sleep 1 up to the timer resolution, 15.6 ms by default. Asking for a
    // 1 ms resolution lets the peak sampler below actually sample about every millisecond.
    [<Runtime.InteropServices.DllImport("winmm.dll")>]
    extern uint32 timeBeginPeriod(uint32 milliseconds)

    let private fullCollect () =
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()

    let private measureOnce (useCache: bool) (schemaText: string) : Measurement =
        let flags = flagsWithCache useCache
        fullCollect ()
        let heapBefore = GC.GetTotalMemory true
        let allocatedBefore = GC.GetTotalAllocatedBytes true

        let mutable peak = heapBefore
        let mutable sampling = true
        let sampler =
            Thread(
                (fun () ->
                    while Volatile.Read &sampling do
                        let heap = GC.GetTotalMemory false
                        if heap > peak then peak <- heap
                        Thread.Sleep 1),
                IsBackground = true
            )
        sampler.Start()

        let watch = Stopwatch.StartNew()
        let root = provide flags schemaText
        let runMs = watch.Elapsed.TotalMilliseconds
        force root
        let totalMs = watch.Elapsed.TotalMilliseconds

        Volatile.Write(&sampling, false)
        sampler.Join()
        let allocated = GC.GetTotalAllocatedBytes true - allocatedBefore
        let heapAfter = GC.GetTotalMemory true
        GC.KeepAlive root

        { RunMs = runMs
          TotalMs = totalMs
          AllocatedMB = mb allocated
          PeakMB = mb (max peak heapAfter - heapBefore)
          RetainedMB = mb (heapAfter - heapBefore) }

    let private median (xs: float list) =
        let sorted = List.sort xs |> List.toArray
        let n = sorted.Length
        if n % 2 = 1 then sorted[n / 2] else (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0

    // Fast schemas: one warm-up run, then repeats until 7 runs or 3 seconds (at least 3 runs).
    // A schema whose first run takes over 20 seconds is measured once; the JIT warm-up is
    // negligible at that length, and repeating it would take hours.
    let private measure (useCache: bool) (schemaText: string) : Measurement * int =
        let first = measureOnce useCache schemaText
        if first.TotalMs > 20_000.0 then
            first, 1
        else
            let budget = Stopwatch.StartNew()
            let rec loop acc =
                let acc = measureOnce useCache schemaText :: acc
                if List.length acc >= 7 || (List.length acc >= 3 && budget.Elapsed.TotalSeconds > 3.0) then acc
                else loop acc
            let runs = loop []
            let med f = runs |> List.map f |> median
            { RunMs = med (fun m -> m.RunMs)
              TotalMs = med (fun m -> m.TotalMs)
              AllocatedMB = med (fun m -> m.AllocatedMB)
              PeakMB = med (fun m -> m.PeakMB)
              RetainedMB = med (fun m -> m.RetainedMB) },
            runs.Length

    let private families : (string * int list * (int -> string)) list =
        [ "objectTree", [ 1; 5; 10; 20; 30; 40; 60; 80; 100; 120; 150 ], Schemas.objectTree
          "refinedArrays", [ 1; 2; 3; 4; 6; 8; 12; 16; 20; 24; 32; 40; 48; 64 ], Schemas.refinedArrays
          "deepChain", [ 1; 2; 4; 6; 8; 10; 12; 14; 16; 18; 20; 30; 40; 60; 80 ], Schemas.deepChain ]

    // A size is skipped for one cache setting once a smaller size took longer than this.
    let private timeLimitMs = 120_000.0

    let private header = "family,size,nodes,cache,runs,runMs,totalMs,allocatedMB,peakMB,retainedMB"

    // Writes results/cache-benchmark.csv with every row, and one file per family and cache setting
    // (results/<family>-<on|off>.csv) for plots.tex, since pgfplots cannot filter rows on text columns.
    // Both are rewritten after every row, so a stopped run keeps what it measured.
    [<EntryPoint>]
    let main args =
        if OperatingSystem.IsWindows() then timeBeginPeriod 1u |> ignore
        let directory = if args.Length > 0 then args[0] else "results"
        Directory.CreateDirectory directory |> ignore
        let all = Collections.Generic.List<string>()
        let perFile = Collections.Generic.Dictionary<string, Collections.Generic.List<string>>()
        let format (x: float) = x.ToString("F2", Globalization.CultureInfo.InvariantCulture)

        let save () =
            File.WriteAllLines(Path.Combine(directory, "cache-benchmark.csv"), Seq.append [ header ] all)
            for KeyValue(key, lines) in perFile do
                File.WriteAllLines(Path.Combine(directory, $"{key}.csv"), Seq.append [ header ] lines)

        for name, sizes, schemaOf in families do
            let tooSlow = Collections.Generic.HashSet<bool>()
            for size in sizes do
                let schemaText = schemaOf size
                let nodes = Schemas.nodeCount schemaText
                for useCache in [ true; false ] do
                    if not (tooSlow.Contains useCache) then
                        let m, runs = measure useCache schemaText
                        let cache = if useCache then "on" else "off"
                        let line =
                            String.Join(
                                ",",
                                [ name; string size; string nodes; cache; string runs
                                  format m.RunMs; format m.TotalMs; format m.AllocatedMB; format m.PeakMB; format m.RetainedMB ]
                            )
                        all.Add line
                        let key = $"{name}-{cache}"
                        if not (perFile.ContainsKey key) then perFile[key] <- Collections.Generic.List()
                        perFile[key].Add line
                        save ()
                        printfn "%s" line
                        if m.TotalMs > timeLimitMs then tooSlow.Add useCache |> ignore
        0
