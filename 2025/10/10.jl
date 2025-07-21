module Codyssi2025Day10

load(file::AbstractString) =
    stack(parse.(Int, split(line)) for line in eachline(file))

partone(grid::Matrix{Int}) =
    minimum(Iterators.flatmap(sum, (eachcol(grid), eachrow(grid))))

function shortestpaths(grid::Matrix{Int}, (height, width) = size(grid))
    shortestpaths = fill(typemax(Int), (height, width))
    shortestpaths[1, 1] = grid[1, 1]
    for x in 2:width
        shortestpaths[1, x] = grid[1, x] + shortestpaths[1, x-1]
    end
    for y in 2:height
        shortestpaths[y, 1] = grid[y, 1] + shortestpaths[y-1, 1]
    end
    for c in CartesianIndex(2, 2):last(keys(grid))
        shortestpaths[c] = grid[c] + min(
            shortestpaths[c-CartesianIndex(0, 1)],
            shortestpaths[c-CartesianIndex(1, 0)]
        )
    end
    shortestpaths
end

parttwo(grid) = last(shortestpaths(grid, (15, 15)))
partthree(grid) = last(shortestpaths(grid))

if isinteractive()
    using REPL
    REPL.activate(Codyssi2025Day10)
elseif !isempty(ARGS)
    grid = load(first(ARGS))
    println(partone(grid))
    paths = shortestpaths(grid)
    println(paths[15, 15])
    println(last(paths))
end

end
