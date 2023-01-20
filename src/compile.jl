# To make it less likely that users measure Records compilation time.
let
    to = Record()
    @recordit to "1" string(1)
end

function _precompile_()
  ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
  @assert Base.precompile(Tuple{typeof(print_timer), typeof(stdout), Record})
  @assert Base.precompile(Tuple{typeof(print_timer), Record})
  @assert Base.precompile(Tuple{typeof(push!), Record, String})
  @assert Base.precompile(Tuple{typeof(reset_timer!), Record})
  @assert Base.precompile(Tuple{typeof(disable_timer!), Record})
  @assert Base.precompile(Tuple{typeof(enable_timer!), Record})
  @assert Base.precompile(Tuple{typeof(complement!), Record})
  @assert Base.precompile(Tuple{typeof(do_accumulate!), RecordData, UInt64, Int64})
  @assert Base.precompile(Tuple{Type{Record}, String})
end
