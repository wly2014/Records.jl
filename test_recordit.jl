push!(LOAD_PATH, joinpath(@__DIR__, "./"))

using Records

to = Record()

function fx()
    return [1,2,3]
end
a = [1,2,3,4,5]
# @recordit to "a" fx()
# for i in 1:10
#     @recordit_debug to "a" a
#     @recordit to "b" zeros(200,10)
#     @recordit to "i" i
#     @recordit to "rand" rand()
# end
for i in 1:10
    @recordit "a" a
    @recordit "b" zeros(200,10)
    @recordit "i" i
    @recordit "rand" rand()
end

# show(to)
# show(Records.get_defaulttimer());
print_record!()

@show getValudof(Records.get_defaulttimer(),"a")
a[1] = 10
@show getValudof(Records.get_defaulttimer(),"a")

nothing