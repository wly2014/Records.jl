# Records

This repository is copied from https://github.com/KristofferC/TimerOutputs.jl, but it is used for recording variables in the running time, which is particularly useful for the optimization solver.

```julia
push!(LOAD_PATH, joinpath(@__DIR__, "./"))

using Records

to = Record()

function fx()
    return [1,2,3]
end
a = [1,2,3,4,5]
for i in 1:10
    @recordit "a" a
    @recordit "b" zeros(200,10)
    @recordit "i" i
    @recordit "rand" rand()
end

print_record!()

@show getValudof(Records.get_defaulttimer(),"a")
```

```julia
 ────────────────────────────────────────────────────────────────────
                            Time                    Allocations      
                   ───────────────────────   ────────────────────────
 Tot / % measured:      5.30s /   0.0%           31.4MiB /   1.0%

 Section   ncalls     time    %tot     avg     alloc    %tot      avg
 ────────────────────────────────────────────────────────────────────
 b             20   0.00ns     - %  0.00ns    313KiB   99.2%  15.7KiB
 a             20   0.00ns     - %  0.00ns   1.76KiB    0.6%    90.0B
 rand          20   0.00ns     - %  0.00ns      360B    0.1%    18.0B
 i             20   0.00ns     - %  0.00ns      280B    0.1%    14.0B
 ────────────────────────────────────────────────────────────────────
```
