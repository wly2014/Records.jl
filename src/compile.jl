# To make it less likely that users measure Records compilation time.
let
    to = Record()
    @recordit to "1" string(1)
end

function _precompile_()
  ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
  @assert Base.precompile(Tuple{typeof(print_record!), typeof(stdout), Record})
  @assert Base.precompile(Tuple{typeof(print_record!), Record})
  @assert Base.precompile(Tuple{typeof(push!), Record, String})
  @assert Base.precompile(Tuple{typeof(reset_record!), Record})
  @assert Base.precompile(Tuple{Type{Record}, String})
end
