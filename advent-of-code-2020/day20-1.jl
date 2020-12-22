# Written by: https://github.com/DearRude/aoc-2020/blob/main/solutions/day-20/part-1.jl

function gentiles(input)::Dict{Int, Array{Bool, 2}}
    tiles::Dict{Int, Array{Bool, 2}} = Dict()
    for tile in 1:12:length(input)
        pixels = zeros(Bool, DIM, DIM)
        for (r_idx, row)=enumerate(input[tile+1:tile+DIM]), (l_idx, el)=enumerate(row)
            el=='#' && (pixels[r_idx, l_idx] = true)
        end
        tiles[parse(Int, input[tile][end-4:end-1])] = pixels
    end
    tiles
end

function findcorns(tiles)
    freq::Dict{Int, Int} = Dict()
    [freq[key1] = get(freq, key1, 0) + 1 for (key1, mat1)=tiles, (key2, mat2)=tiles
        if key1 != key2 && matchtwo(mat1, mat2)]
    [key for (key, val) in freq if val==2] |> prod
end

function matchtwo(firtile::Array{Bool, 2}, sectile::Array{Bool, 2})::Bool
    eadges(mat)::Array{Int} = hcat(mat[:,1], mat[:,end], mat[end,:], mat[1,:])'
    revcat(mat)::Array{Int} = vcat(mat, reverse(mat, dims=2))
    [firrow==secrow for firrow=firtile|>eadges|>revcat|>eachrow,
        secrow=sectile|>eadges|>eachrow] |> any
end

DIM = 10
readlines("day20-test.txt") |> gentiles |> findcorns |> println
