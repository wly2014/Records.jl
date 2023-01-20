using Records
using Test

import Records: DEFAULT_TIMER, ncalls, flatten,
                     prettytime, prettymemory, prettypercent, prettycount, todict

reset_timer!()

# Timing from modules that don't import much
baremodule NoImports
    using Records
    using Base: sleep
    @recordit "baresleep" sleep(0.1)
end

@testset "Record" begin

@test "baresleep" in keys(DEFAULT_TIMER.inner_timers)

to = Record()
@recordit to "sleep" sleep(0.1)
@recordit "sleep" sleep(0.1)

@test "sleep" in keys(to.inner_timers)
@test "sleep" in keys(DEFAULT_TIMER.inner_timers)

@recordit to "multi statement" begin
1+1
sleep(0.1)
end

@recordit "multi statement" begin
1+1
sleep(0.1)
end

@test "multi statement" in keys(to.inner_timers)
@test "multi statement" in keys(DEFAULT_TIMER.inner_timers)

@recordit to "sleep" sleep(0.1)
@recordit to "sleep" sleep(0.1)
@recordit to "sleep" sleep(0.1)

@recordit "sleep" sleep(0.1)
@recordit "sleep" sleep(0.1)
@recordit "sleep" sleep(0.1)

@test haskey(to, "sleep")
@test !haskey(to, "slep")
@test ncalls(to["sleep"]) == 4
@test ncalls(DEFAULT_TIMER["sleep"]) == 4


# Check reset works
reset_timer!(to)
reset_timer!()

@test length(keys(to.inner_timers)) == 0
@test length(keys(DEFAULT_TIMER.inner_timers)) == 0


# Check return values get propagated
function foo(a)
    a+a
end

to2 = Record()

a = @recordit to2 "foo" foo(5)
b = @recordit "foo" foo(5)

@test a === 10
@test b === 10
@test "foo" in collect(keys(to2.inner_timers))
@test "foo" in collect(keys(DEFAULT_TIMER.inner_timers))

# Test nested
c = @recordit to2 "nest 1" begin
    sleep(0.01)
    @recordit to2 "nest 2" sleep(0.02)
    @recordit to2 "nest 2" sleep(0.02)
    5
end

d = @recordit "nest 1" begin
    sleep(0.01)
    @recordit "nest 2" sleep(0.02)
    @recordit "nest 2" sleep(0.02)
    5
end

@test ncalls(to2["nest 1"]) == 1
@test ncalls(to2["nest 1"]["nest 2"]) == 2
@test ncalls(DEFAULT_TIMER["nest 1"])== 1
@test ncalls(DEFAULT_TIMER["nest 1"]["nest 2"]) == 2
@test c === 5
@test d == 5

# test throws
function foo2(v)
    @recordit to "throwing" begin
        sleep(0.01)
        print(v[6]) # OOB
    end
end

function foo3(v)
    @recordit "throwing" begin
        sleep(0.01)
        print(v[6]) # OOB
    end
end

try
    foo2(rand(5))
catch e
    isa(e, BoundsError) || rethrow(e)
end

try
    foo3(rand(5))
catch e
    isa(e, BoundsError) || rethrow(e)
end

@test "throwing" in keys(to.inner_timers)
@test "throwing" in keys(DEFAULT_TIMER.inner_timers)

reset_timer!(to)

@recordit to "foo" begin
    sleep(0.05)
    @recordit to "bar" begin
        @recordit to "foo" sleep(0.05)
        @recordit to "foo" sleep(0.05)
        @recordit to "baz" sleep(0.05)
        @recordit to "bar" sleep(0.05)
    end
    @recordit to "bur" sleep(0.025)
end
@recordit to "bur" sleep(0.025)

tom = flatten(to)
@test ncalls(tom["foo"]) == 3
@test ncalls(tom["bar"]) == 2
@test ncalls(tom["bur"]) == 2
@test ncalls(tom["baz"]) == 1

function many_loops()
    for i in 1:10^7
        @recordit to "loop" 1+1
    end
end

many_loops()

a = 3
@recordit to "a$a"  1+1
@recordit "a$a" 1+1

@test "a3" in collect(keys(to.inner_timers))
@test "a3" in collect(keys(DEFAULT_TIMER.inner_timers))

reset_timer!(DEFAULT_TIMER)
toz = Record()
@recordit toz "foo" 1+1
reset_timer!(toz)
@recordit toz "foo" 1+1
@test "foo" in keys(toz.inner_timers)

tof = Record()
@recordit tof ff1(x) = x
@recordit tof ff2(x)::Float64 = x
@recordit tof function ff3(x) x end
@recordit tof function ff4(x)::Float64 x end

@recordit ff5(x) = x
@recordit ff6(x)::Float64 = x
@recordit function ff7(x) x end
@recordit function ff8(x)::Float64 x end

@recordit ff9(x::T) where {T} = x
@recordit (ff10(x::T)::Float64) where {T} = x
@recordit function ff11(x::T) where {T} x end
@recordit function ff12(x::T)::Float64 where {T} x end

@recordit "foo" ff13(x::T) where {T} = x
@recordit "bar" (ff14(x::T)::Float64) where {T} = x
@recordit "baz" function ff15(x::T) where {T} x end
@recordit "quz" function ff16(x::T)::Float64 where {T} x end

@recordit tof "foo" ff17(x::T) where {T} = x
@recordit tof "bar" (ff18(x::T)::Float64) where {T} = x
@recordit tof "baz" function ff19(x::T) where {T} x end
@recordit tof "quz" function ff20(x::T)::Float64 where {T} x end

for i in 1:2
    @test ff1(1) === 1
    @test ff2(1) === 1.0
    @test ff3(1) === 1
    @test ff4(1) === 1.0
    @test ff5(1) === 1
    @test ff6(1) === 1.0
    @test ff7(1) === 1
    @test ff8(1) === 1.0
    @test ff9(1) === 1
    @test ff10(1) === 1.0
    @test ff11(1) === 1
    @test ff12(1) === 1.0
    @test ff13(1) === 1
    @test ff14(1) === 1.0
    @test ff15(1) === 1
    @test ff16(1) === 1.0
    @test ff17(1) === 1
    @test ff18(1) === 1.0
    @test ff19(1) === 1
    @test ff20(1) === 1.0
end

@test ncalls(tof["ff1"]) == 2
@test ncalls(tof["ff2"]) == 2
@test ncalls(tof["ff3"]) == 2
@test ncalls(tof["ff4"]) == 2
@test ncalls(tof["foo"]) == 2
@test ncalls(tof["bar"]) == 2
@test ncalls(tof["baz"]) == 2
@test ncalls(tof["quz"]) == 2

@test ncalls(DEFAULT_TIMER["ff5"]) == 2
@test ncalls(DEFAULT_TIMER["ff6"]) == 2
@test ncalls(DEFAULT_TIMER["ff7"]) == 2
@test ncalls(DEFAULT_TIMER["ff8"]) == 2
@test ncalls(DEFAULT_TIMER["ff9"]) == 2
@test ncalls(DEFAULT_TIMER["ff10"]) == 2
@test ncalls(DEFAULT_TIMER["ff11"]) == 2
@test ncalls(DEFAULT_TIMER["ff12"]) == 2
@test ncalls(DEFAULT_TIMER["foo"]) == 2
@test ncalls(DEFAULT_TIMER["bar"]) == 2
@test ncalls(DEFAULT_TIMER["baz"]) == 2
@test ncalls(DEFAULT_TIMER["quz"]) == 2

function foo()
    reset_timer!()
    @recordit "asdf" bar()
end

bar() = print_timer()

foo()

io = IOBuffer()
show(io, to)
show(io, to; allocations = false)
show(io, to; allocations = false, compact = true)
show(io, to; sortby = :ncalls)
show(io, to; sortby = :time)
show(io, to; sortby = :allocations)
show(io, to; sortby = :name)
show(io, to; sortby = :firstexec)
show(io, to; linechars = :ascii)
show(io, to; title = "A short title")
show(io, to; title = "A very long title that will be truncated")

# issue 22: edge cases for rounding
for (t, str) in ((9999,    "10.0μs"), (99999,    " 100μs"),
                 (9999999, "10.0ms"), (99999999, " 100ms"))
    @test prettytime(t) == str
end
for (b, str) in ((9.999*1024,   "10.0KiB"), (99.999*1024,   " 100KiB"),
                 (9.999*1024^2, "10.0MiB"), (99.999*1024^2, " 100MiB"),
                 (9.999*1024^3, "10.0GiB"), (99.999*1024^3, " 100GiB"))
    @test prettymemory(b)   == str
end
for (num, den, str) in ((0.9999, 1, "100.0%"), (0.09999, 1, " 10.0%"))
    @test prettypercent(num, den) == str
end
for (t, str) in ((9.999*1024,   "10.0KiB"), (99.999*1024,   " 100KiB"),
                 (9.999*1024^2, "10.0MiB"), (99.999*1024^2, " 100MiB"),
                 (9.999*1024^3, "10.0GiB"), (99.999*1024^3, " 100GiB"))
    @test prettymemory(t)   == str
end
for (c, str) in ((9999, "10.0k"), (99999, "100k"),
                 (9999999, "10.0M"), (99999999, "100M"),
                 (9999999999, "10.0B"), (99999999999, "100B"))
    @test prettycount(c) == str
end

# `continue` inside a timeit section
to_continue = Record()
function continue_test()
   for i = 1:10
       @recordit to_continue "x" @recordit to_continue "test" begin
           continue
       end
   end
end
continue_test()
@test isempty(to_continue.inner_timers["x"].inner_timers["test"].inner_timers)


# Test @recordit_debug
to_debug = Record()
function debug_test()
    @recordit_debug to_debug "sleep" sleep(0.001)
end

Records.disable_debug_timings(@__MODULE__)
debug_test()
@test !("sleep" in keys(to_debug.inner_timers))
Records.enable_debug_timings(@__MODULE__)
debug_test()
@test "sleep" in keys(to_debug.inner_timers)


# Test functional-form @recordit_debug with @eval'ed functions
to_debug = Record()

@recordit_debug to_debug function baz(x, y)
    @recordit_debug to_debug "sleep" sleep(0.001)
    return x + y * x
end

Records.disable_debug_timings(@__MODULE__)
baz(1, 2.0)
@test isempty(to_debug.inner_timers)

Records.enable_debug_timings(@__MODULE__)
baz(1, 2.0)
@test "baz" in keys(to_debug.inner_timers)
@test "sleep" in keys(to_debug.inner_timers["baz"].inner_timers)
Records.disable_debug_timings(@__MODULE__)

to = Record()
@recordit to "section1" sleep(0.02)
@recordit to "section2" begin
    @recordit to "section2.1" sleep(0.1)
    sleep(0.01)
end
Records.complement!(to)

tom = flatten(to)
@test ncalls(tom["~section2~"]) == 1

end # testset

struct Simulation
   timer::Record
   # state
end

@testset "Timer from argument" begin
    get_timer(sim) = sim.timer
    @recordit get_timer(sim) function step!(sim::Simulation)
        # important computation
    end
    sim = Simulation(Records.Record())
    step!(sim)
    @test Records.ncalls(sim.timer["step!"]) == 1
    step!(sim)
    @test Records.ncalls(sim.timer["step!"]) == 2

    @recordit get_timer(args...; kw...) step2!(args...; kw...) = nothing
    step2!(sim)
    @test Records.ncalls(sim.timer["step!"]) == 2
    @test Records.ncalls(sim.timer["step2!"]) == 1
    step2!(sim)
    @test Records.ncalls(sim.timer["step2!"]) == 2
end

# default timer without explicitly loading Records
Records.reset_timer!()
module TestModule
    using Records: @recordit
    foo(x) = x
    @recordit "foo" foo(1)
end
@test "foo" in keys(DEFAULT_TIMER.inner_timers)
Records.reset_timer!()

# Test sharing timers between modules
@test !haskey(Records._timers, "TestModule2")
@test !haskey(Records._timers, "my_timer")

to = get_timer("my_timer")
@recordit to "foo" sleep(0.1)
@test ncalls(get_timer("my_timer")["foo"]) == 1

module TestModule2
    using Records: @recordit, get_timer
    foo(x) = x
    @recordit get_timer("TestModule2") "foo" foo(1)
    @recordit get_timer("my_timer") "foo" foo(1)
end

# Timer from module is accessible to root
@test haskey(Records._timers, "TestModule2")
@test ncalls(get_timer("TestModule2")["foo"]) == 1
# Timer from root is accessible to module
@test ncalls(get_timer("my_timer")["foo"]) == 2

# Broken
#=
# Type inference with @recordit_debug
@recordit_debug function make_zeros()
   dims = (3, 4)
   zeros(dims)
end
@inferred make_zeros()
Records.enable_debug_timings(@__MODULE__)
@inferred make_zeros()
=#

to = Record()
@recordit_debug to function f(x)
   g(x) = 2x
   g(x)
end
@test f(3) == 6
Records.enable_debug_timings(@__MODULE__)
@test f(3) == 6
Records.disable_debug_timings(@__MODULE__)

@testset "Not too many allocations #59" begin
    function doit(timer, n)
        ret = 0
        for i in 1:n
            @recordit timer "depth0" begin
                @recordit timer "depth1" begin
                    @recordit timer "depth2" begin
                        ret += sin(i)
                    end
                    @recordit timer "depth2b" begin
                        ret += cos(i)
                    end
                end
                @recordit timer "depth1b" begin

                end
            end
        end
        ret
    end

    to = Record()
    doit(to, 1)
    a0 = Records.allocated(to["depth0"])
    a1 = Records.allocated(to["depth0"]["depth1"])
    a2 = Records.allocated(to["depth0"]["depth1"]["depth2"])

    to = Record()
    doit(to, 100000)

    to0 = to["depth0"]
    to1 = to0["depth1"]
    to1b = to0["depth1b"]
    to2 = to1["depth2"]
    to2b = to1["depth2b"]

    # test that leaf timers add zero allocations
    # and other timers only add allocations once
    @test Records.allocated(to0) == a0
    @test Records.allocated(to1) == a1
    @test Records.allocated(to2) == a2
    @test Records.allocated(to1b) == 0
    @test Records.allocated(to2) == 0
    @test Records.allocated(to2b) == 0
end

@testset "disable enable" begin
    to = Record()
    ff1() = @recordit to "ff1" 1+1
    ff1()
    @test ncalls(to["ff1"]) == 1
    disable_timer!(to)
    ff1()
    @test ncalls(to["ff1"]) == 1
    enable_timer!(to)
    ff1()
    @test ncalls(to["ff1"]) == 2
    @notimeit to ff1()
    ff1()
    @test ncalls(to["ff1"]) == 3
end

# Type inference with @recordit_debug
@recordit_debug function make_zeros()
   dims = (3, 4)
   zeros(dims)
end
@inferred make_zeros()
Records.enable_debug_timings(@__MODULE__)
@inferred make_zeros()

@testset "merge" begin
    to1 = Record()
    to2 = Record()
    to3 = Record()

    @recordit to1 "foo" identity(nothing)
    @recordit to1 "baz" identity(nothing)
    @recordit to1 "foobar" begin
        @recordit to1 "foo" identity(nothing)
        @recordit to1 "baz" identity(nothing)
    end

    @recordit to1 "bar" identity(nothing)
    @recordit to2 "baz" identity(nothing)
    @recordit to2 "foobar" begin
        @recordit to2 "bar" identity(nothing)
        @recordit to2 "baz" identity(nothing)
    end

    @recordit to3 "bar" identity(nothing)

    to_merged = merge(to1, to2, to3)
    merge!(to1, to2, to3)

    for to in [to1, to_merged]
        @test "foo" in collect(keys(to.inner_timers))
        @test "bar" in collect(keys(to.inner_timers))
        @test "foobar" in collect(keys(to.inner_timers))

        subto = to["foobar"]
        @test "foo" in collect(keys(subto.inner_timers))
        @test "bar" in collect(keys(subto.inner_timers))

        @test ncalls(to["foo"]) == 1
        @test ncalls(to["bar"]) == 2
        @test ncalls(to["baz"]) == 2

        @test ncalls(subto["foo"]) == 1
        @test ncalls(subto["bar"]) == 1
        @test ncalls(subto["baz"]) == 2
    end
end

# Issue #118
let to = Record()
    @recordit to "foo" identity(nothing)
    @recordit to "foobar" begin
        @recordit to "foo" identity(nothing)
        @recordit to "baz" identity(nothing)
    end
    @recordit to "baz" identity(nothing)

    @test ncalls(to.inner_timers["foo"]) == 1
    @test ncalls(to.inner_timers["foobar"]) == 1
    @test ncalls(to.inner_timers["foobar"].inner_timers["foo"]) == 1
    @test ncalls(to.inner_timers["foobar"].inner_timers["baz"]) == 1
    @test ncalls(to.inner_timers["baz"]) == 1
end

@testset "sortby firstexec" begin
    to = Record()
    @recordit to "cccc" sleep(0.1)
    @recordit to "cccc" sleep(0.1)
    @recordit to "bbbb" sleep(0.1)
    @recordit to "aaaa" sleep(0.1)
    @recordit to "cccc" sleep(0.1)

    table = sprint((io, to)->show(io, to, sortby = :firstexec), to)
    @test match(r"cccc", table).offset < match(r"bbbb", table).offset < match(r"aaaa", table).offset

    to = Record()
    @recordit to "group" begin
        @recordit to "aaaa" sleep(0.1)
        @recordit to "nested_group" begin sleep(0.1)
            @recordit to "bbbb" sleep(0.1)
            @recordit to "cccc" sleep(0.1)
        end
    end

    table = sprint((io, to)->show(io, to, sortby = :firstexec), to)
    @test match(r"aaaa", table).offset < match(r"bbbb", table).offset < match(r"cccc", table).offset
end

@static if isdefined(Threads, Symbol("@spawn"))
@testset "merge at custom points during multithreading" begin
    to = Record()
    @recordit to "1" begin
        @recordit to "1.1" sleep(0.1)
        @recordit to "1.2" sleep(0.1)
        @recordit to "1.3" sleep(0.1)
    end

    @sync begin
        @recordit to "2" Threads.@spawn begin
            to2 = Record()
            @recordit to2 "2.1" sleep(0.1)
            @recordit to2 "2.2" sleep(0.1)
            @recordit to2 "2.3" sleep(0.1)
            merge!(to, to2, tree_point = ["2"])
        end

        @recordit to "3" Threads.@spawn begin
            to3 = Record()
            @sync begin
                @recordit to3 "3.1" Threads.@spawn begin
                    to31 = Record()
                    @recordit to31 "3.1.1" sleep(0.1)
                    @recordit to31 "3.1.2" sleep(0.1)
                    @recordit to31 "3.1.3" sleep(0.1)
                    merge!(to3, to31, tree_point = ["3.1"])
                end
                @recordit to3 "3.2" Threads.@spawn begin
                    to32 = Record()
                    @recordit to32 "3.2.1" sleep(0.1)
                    @recordit to32 "3.2.2" sleep(0.1)
                    @recordit to32 "3.2.3" sleep(0.1)
                    merge!(to3, to32, tree_point = ["3.2"])
                end
            end
            merge!(to, to3, tree_point = ["3"])
        end
    end

    @test "1" in collect(keys(to.inner_timers))
    @test ncalls(to.inner_timers["1"]) == 1
    @test "2" in collect(keys(to.inner_timers))
    @test ncalls(to.inner_timers["2"]) == 1
    @test "3" in collect(keys(to.inner_timers))
    @test ncalls(to.inner_timers["3"]) == 1
    @test !in("1.1", collect(keys(to.inner_timers)))
    @test !in("2.1", collect(keys(to.inner_timers)))
    @test !in("3.1", collect(keys(to.inner_timers)))
    @test !in("3.1.1", collect(keys(to.inner_timers)))
    @test !in("3.2", collect(keys(to.inner_timers)))
    @test !in("3.2.1", collect(keys(to.inner_timers)))

    to1 = to.inner_timers["1"]
    @test "1.1" in collect(keys(to1.inner_timers))
    @test ncalls(to1.inner_timers["1.1"]) == 1

    to2 = to.inner_timers["2"]
    @test "2.1" in collect(keys(to2.inner_timers))
    @test ncalls(to2.inner_timers["2.1"]) == 1
    @test !in("3.1", collect(keys(to2.inner_timers)))

    to3 = to.inner_timers["3"]
    @test "3.1" in collect(keys(to3.inner_timers))
    @test ncalls(to3.inner_timers["3.1"]) == 1
    @test "3.2" in collect(keys(to3.inner_timers))
    @test ncalls(to3.inner_timers["3.2"]) == 1
    @test !in("2.1", collect(keys(to3.inner_timers)))

    to31 = to3.inner_timers["3.1"]
    @test "3.1.1" in collect(keys(to31.inner_timers))
    @test ncalls(to31.inner_timers["3.1.1"]) == 1
    @test !in("3.2.1", collect(keys(to31.inner_timers)))

    to32 = to3.inner_timers["3.2"]
    @test "3.2.1" in collect(keys(to32.inner_timers))
    @test ncalls(to32.inner_timers["3.2.1"]) == 1
    @test !in("3.1.1", collect(keys(to32.inner_timers)))
end
end

@testset "Serialization" begin
    # Setup a timer
    to = Record()
    @recordit to "foo" identity(nothing)
    @recordit to "foobar" begin
        @recordit to "foo" identity(nothing)
        @recordit to "baz" identity(nothing)
    end
    @recordit to "baz" identity(nothing)


    function compare(to, d)
        @test Records.tottime(to) == d["total_time_ns"]
        @test Records.ncalls(to) == d["n_calls"]
        @test Records.totallocated(to) == d["total_allocated_bytes"]
        @test Records.allocated(to) == d["allocated_bytes"]
        @test Records.time(to) == d["time_ns"]
        for ((k1, timer), (k2, obj)) in zip(to.inner_timers, d["inner_timers"])
            @test k1 == k2
            compare(timer, obj)
        end
    end
    
    compare(to, todict(to))
end

@testset "InstrumentedFunctions" begin
    to = Record()
    f = to(x -> x^2, "f")
    @test isempty(to.inner_timers)
    f(1)
    @test ncalls(to.inner_timers["f"]) == 1
    h = to(x -> f(x) + 1, "h")
    h(1)
    @test ncalls(to.inner_timers["h"]) == 1
    @test ncalls(to.inner_timers["h"].inner_timers["f"]) == 1
    s = x -> x+1
    t = to(s)
    t(1)
    ncalls(to.inner_timers[repr(s)]) == 1
end
