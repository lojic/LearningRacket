# Solution by tckmn (lightly modified by me)
# https://github.com/tckmn/polyaoc-2020/blob/master/10/jl/10.jl

input = sort!(parse.(Int, eachline("day10.txt")))
pushfirst!(input, 0)
push!(input, input[end]+3)

# part 1
diffs = diff(input)
println(count(diffs .== 1) * count(diffs .== 3))

# part 2
const memo = Dict{Int,Int}()

function ways(n)
  if n ∉ input
    0
  elseif n == input[end]
    1
  else
    get!(memo, n) do
      sum(ways, n+1:n+3)
    end
  end
end

println(ways(0))
