module Records

using ExprTools

import Base: show, time_ns
export Record, @recordit, @recordit_debug, reset_record!, print_record!,
                    get_record, getValudof

# https://github.com/JuliaLang/julia/pull/33717
if VERSION < v"1.4.0-DEV.475"
    gc_bytes() = Base.gc_bytes()
else
    function gc_bytes()
        b = Ref{Int64}(0)
        Base.gc_bytes(b)
        return b[]
    end
end

using Printf

include("Record.jl")
include("show.jl")
include("utilities.jl")

if Base.VERSION >= v"1.4.2"
    include("compile.jl")
    _precompile_()
end

function __init__()
    # Reset DEFAULT_TIMER; otherwise starting time is the time of precompile
    reset_record!()
end

end # module
