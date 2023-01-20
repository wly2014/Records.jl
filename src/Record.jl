############
# RecordData #
############
mutable struct RecordData
    ncalls::Int
    time::Int64
    allocs::Int64
    firstexec::Int64
    data::Vector
end
RecordData(ncalls, time, allocs, data) = RecordData(ncalls, time, allocs, time, data)
RecordData(ncalls, time, allocs) = RecordData(ncalls, time, allocs, time, [])
Base.copy(td::RecordData) = RecordData(td.ncalls, td.time, td.allocs, td.data)
RecordData() = RecordData(0, 0, 0, time_ns(), [])

function Base.:+(self::RecordData, other::RecordData)
    RecordData(self.ncalls + other.ncalls,
             self.time + other.time,
             self.allocs + other.allocs,
             min(self.firstexec, other.firstexec),
             [self.data..., other.data...])
end

###############
# Record #
###############
mutable struct Record
    start_data::RecordData
    accumulated_data::RecordData
    inner_timers::Dict{String,Record}
    timer_stack::Vector{Record}
    name::String
    flattened::Bool
    enabled::Bool
    totmeasured::Tuple{Int64,Int64}
    prev_timer_label::String
    prev_timer::Union{Record,Nothing}

    function Record(label::String = "root")
        start_data = RecordData(0, time_ns(), gc_bytes())
        accumulated_data = RecordData()
        inner_timers = Dict{String,Record}()
        timer_stack = Record[]
        return new(start_data, accumulated_data, inner_timers, timer_stack, label, false, true, (0, 0), "", nothing)
    end

    # Jeez...
    Record(start_data, accumulated_data, inner_timers, timer_stack, name, flattened, enabled, totmeasured, prev_timer_label,
    prev_timer) = new(start_data, accumulated_data, inner_timers, timer_stack, name, flattened, enabled, totmeasured, prev_timer_label,
    prev_timer)

end

Base.copy(to::Record) = Record(copy(to.start_data), copy(to.accumulated_data), copy(to.inner_timers),
                                         copy(to.timer_stack), to.name, to.flattened, to.enabled, to.totmeasured, "", nothing)

const DEFAULT_TIMER = Record()
const _timers = Dict{String, Record}("Default" => DEFAULT_TIMER)
const _timers_lock = ReentrantLock() # needed for adding new timers on different threads
"""
    get_timer(name::String)

Returns the `Record` associated with `name`.
If no timers are associated with `name`, a new `Record` will be created.
"""
function get_timer(name::String)
    lock(_timers_lock) do
        if !haskey(_timers, name)
            _timers[name] = Record(name)
        end
        return _timers[name]
    end
end

# push! and pop!
function Base.push!(to::Record, label::String)
    if length(to.timer_stack) == 0 # Root section
        current_timer = to
    else # Not a root section
        current_timer = to.timer_stack[end]
    end
    # Fast path
    if current_timer.prev_timer_label == label
        timer = current_timer.prev_timer
    else
        maybe_timer = get(current_timer.inner_timers, label, nothing)
        # this could be implemented more elegant using
        # get!(() -> Record(label), current_timer.inner_timers, label)
        # however that causes lots of allocations in
        # julia v1.3
        if maybe_timer === nothing
            timer = Record(label)
            current_timer.inner_timers[label] = timer
        else
            timer = maybe_timer
        end
    end
    timer = timer::Record
    current_timer.prev_timer_label = label
    current_timer.prev_timer = timer

    push!(to.timer_stack, timer)
    return timer.accumulated_data
end

Base.pop!(to::Record) = pop!(to.timer_stack)

# Only sum the highest parents
function totmeasured(to::Record)
    t, b = Int64(0), Int64(0)
    for section in values(to.inner_timers)
        timedata = section.accumulated_data
        t += timedata.time
        # b += timedata.allocs
        b += Base.summarysize(timedata.data)
    end
    return t, b
end

function longest_name(to::Record, indent = 0)
    m = textwidth(to.name) + indent
    for inner_timer in values(to.inner_timers)
        m = max(m, longest_name(inner_timer, indent + 2))
    end
    return m
end


# merging timer outputs
const merge_lock = ReentrantLock() # needed for merges of objects on different threads

Base.merge(others::Record...) = merge!(Record(), others...)
function Base.merge!(self::Record, others::Record...; tree_point = String[])
    lock(merge_lock) do
        for other in others
            self.accumulated_data += other.accumulated_data
            its = self.inner_timers
            for point in tree_point
                its = its[point].inner_timers
            end
            _merge(its, other.inner_timers)
        end
        return self
    end
end
function _merge(self::Dict{String,Record}, other::Dict{String,Record})
    for key in keys(other)
        if haskey(self, key)
            self[key].accumulated_data += other[key].accumulated_data
            _merge(self[key].inner_timers, other[key].inner_timers)
        else
            self[key] = deepcopy(other[key])
        end
    end
end

#######
# API #
#######

# Accessors
ncalls(to::Record)    = to.accumulated_data.ncalls
allocated(to::Record) = to.accumulated_data.allocs
time(to::Record) = to.accumulated_data.time
totallocated(to::Record) = totmeasured(to)[2]
tottime(to::Record) = totmeasured(to)[1]

time() = time(DEFAULT_TIMER)
ncalls() = ncalls(DEFAULT_TIMER)
allocated() = allocated(DEFAULT_TIMER)
totallocated() = totmeasured(DEFAULT_TIMER)[2]
tottime() = totmeasured(DEFAULT_TIMER)[1]

get_defaulttimer() = DEFAULT_TIMER
Base.@deprecate get_defaultimer get_defaulttimer

# Macro
macro recordit(args...)
    return timer_expr(__module__, false, args...)
end

macro recordit_debug(args...)
    if !isdefined(__module__, :timeit_debug_enabled)
        Core.eval(__module__, :(timeit_debug_enabled() = false))
    end

    return timer_expr(__module__, true, args...)
end

function enable_debug_timings(m::Module)
    if !getfield(m, :timeit_debug_enabled)()
        Core.eval(m, :(timeit_debug_enabled() = true))
    end
end
function disable_debug_timings(m::Module)
    if getfield(m, :timeit_debug_enabled)()
        Core.eval(m, :(timeit_debug_enabled() = false))
    end
end

timer_expr(args...) = throw(ArgumentError("invalid macro usage for @recordit, use as @recordit [to] label codeblock"))

function is_func_def(f)
    if isa(f, Expr) && (f.head === :function || Base.is_short_function_def(f))
        return true
    else
        return false
    end
end

function timer_expr(m::Module, is_debug::Bool, ex::Union{Symbol, Expr})
    is_func_def(ex) && return timer_expr_func(m, is_debug, :($(Records.DEFAULT_TIMER)), ex)
    return esc(_timer_expr(m, is_debug, :($(Records).DEFAULT_TIMER), ex))
end

function timer_expr(m::Module, is_debug::Bool, label_or_to, ex::Union{Symbol, Expr})
    is_func_def(ex) && return timer_expr_func(m, is_debug, label_or_to, ex)
    return esc(_timer_expr(m, is_debug, :($(Records).DEFAULT_TIMER), label_or_to, ex))
end

function timer_expr(m::Module, is_debug::Bool, label::String, ex::Union{Symbol, Expr})
    is_func_def(ex) && return timer_expr_func(m, is_debug, :($(Records).DEFAULT_TIMER), ex, label)
    return esc(_timer_expr(m, is_debug, :($(Records).DEFAULT_TIMER), label, ex))
end

function timer_expr(m::Module, is_debug::Bool, to, label, ex::Union{Symbol, Expr})
    is_func_def(ex) && return timer_expr_func(m, is_debug, to, ex, label)
    return esc(_timer_expr(m, is_debug, to, label, ex))
end

function _timer_expr(m::Module, is_debug::Bool, to::Union{Symbol, Expr, Record}, label, ex::Union{Symbol, Expr})
    @gensym local_to enabled accumulated_data b₀ t₀ val
    timeit_block = quote
        $local_to = $to
        $enabled = $local_to.enabled
        if $enabled
            $accumulated_data = $(push!)($local_to, $label)
        end
        $b₀ = $(gc_bytes)()
        $t₀ = $(time_ns)()
        $(Expr(:tryfinally,
            :($val = $ex),
            quote
                if $enabled
                    $(do_accumulate!)($accumulated_data, $t₀, $b₀)
                    $(push!)($accumulated_data.data, $val)
                    $(pop!)($local_to)
                end
            end))
        $val
    end

    if is_debug
        return quote
            if $m.timeit_debug_enabled()
                $timeit_block
            else
                $ex
            end
        end
    else
        return timeit_block
    end
end

function timer_expr_func(m::Module, is_debug::Bool, to, expr::Union{Symbol, Expr}, label=nothing)
    expr = macroexpand(m, expr)
    def = splitdef(expr)

    label === nothing && (label = string(def[:name]))

    def[:body] = if is_debug
        quote
            @inline function inner()
                $(def[:body])
            end
            $(_timer_expr(m, is_debug, to, label, :(inner())))
        end
    else
        _timer_expr(m, is_debug, to, label, def[:body])
    end

    return esc(combinedef(def))
end

function do_accumulate!(accumulated_data, t₀, b₀)
    # accumulated_data.time += time_ns() - t₀
    # accumulated_data.allocs += gc_bytes() - b₀
    accumulated_data.ncalls += 1
end


reset_timer!() = reset_timer!(DEFAULT_TIMER)
function reset_timer!(to::Record)
    to.inner_timers = Dict{String,Record}()
    to.start_data = RecordData(0, time_ns(), gc_bytes())
    to.accumulated_data = RecordData()
    to.prev_timer_label = ""
    to.prev_timer = nothing
    resize!(to.timer_stack, 0)
    return to
end

# We can remove this now that the @recordit macro is exception safe.
# Doesn't hurt to keep it for a while though
timeit(f::Function, label::String) = timeit(f, DEFAULT_TIMER, label)
function timeit(f::Function, to::Record, label::String)
    accumulated_data = push!(to, label)
    b₀ = gc_bytes()
    t₀ = time_ns()
    local val
    try
        val = f()
    finally
        accumulated_data.time += time_ns() - t₀
        accumulated_data.allocs += gc_bytes() - b₀
        accumulated_data.ncalls += 1
        pop!(to)
    end
    return val
end

Base.haskey(to::Record, name::String) = haskey(to.inner_timers, name)
Base.getindex(to::Record, name::String) = to.inner_timers[name]

function getValudof(to::Record, name::String)
    if !haskey(to, name)
        error("to has not key ", name)
    else
        return to.inner_timers[name].accumulated_data.data
    end
end

function flatten(to::Record)
    t, b = totmeasured(to)
    inner_timers = Dict{String,Record}()
    for inner_timer in values(to.inner_timers)
        _flatten!(inner_timer, inner_timers)
    end
    toc = copy(to)
    return Record(toc.start_data, toc.accumulated_data, inner_timers, Record[], "Flattened", true, true, (t, b), "", to)
end


function _flatten!(to::Record, inner_timers::Dict{String,Record})
    for inner_timer in values(to.inner_timers)
        _flatten!(inner_timer, inner_timers)
    end

    if haskey(inner_timers, to.name)
        timer = inner_timers[to.name]
        timer.accumulated_data += to.accumulated_data
    else
        toc = copy(to)
        toc.inner_timers = Dict{String,Record}()
        inner_timers[toc.name] = toc
    end
end

enable_timer!(to::Record=DEFAULT_TIMER) = to.enabled = true
disable_timer!(to::Record=DEFAULT_TIMER) = to.enabled = false


# Macro to selectively disable timer for expression
macro notimeit(args...)
    notimeit_expr(args...)
end

# Default function throws an error for the benefit of the user
notimeit_expr(args...) = throw(ArgumentError("invalid macro usage for @notimeit, use as @notimeit [to] codeblock"))

complement!() = complement!(DEFAULT_TIMER)
function complement!(to::Record)
    if length(to.inner_timers) == 0
        return nothing
    end
    tot_time = to.accumulated_data.time
    tot_allocs = to.accumulated_data.allocs
    for timer in values(to.inner_timers)
        tot_time -= timer.accumulated_data.time
        tot_allocs -= timer.accumulated_data.allocs
        complement!(timer)
    end
    tot_time = max(tot_time, 0)
    tot_allocs = max(tot_allocs, 0)
    if !(to.name in ["root", "Flattened"])
        name = string("~", to.name, "~")
        timer = Record(to.start_data, RecordData(max(1,to.accumulated_data.ncalls), tot_time, tot_allocs), Dict{String,Record}(), Record[], name, false, true, (tot_time, tot_allocs), to.name, to)
        to.inner_timers[name] = timer
    end
    return to
end

# If @notimeit was called without a Record instance, use default timer
notimeit_expr(ex::Expr) = notimeit_expr(:($(Records.DEFAULT_TIMER)), ex)

# Disable timer, evaluate expression, restore timer to previous value, and return expression result
function notimeit_expr(to, ex::Expr)
    return quote
        local to = $(esc(to))
        local enabled = to.enabled
        $(disable_timer!)(to)
        local val
        $(Expr(:tryfinally,
            :(val = $(esc(ex))),
            quote
                if enabled
                    $(enable_timer!)(to)
                end
            end))
        val
    end
end
