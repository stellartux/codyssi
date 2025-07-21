module Codyssi2024Day4

function load(file::AbstractString)
    edges = split.(eachline(file), " <-> ")
    graph = Dict{Symbol,Vector{Symbol}}()
    addtograph!(g, l, r) = push!(get!(g, l, Symbol[]), r)
    for (left, right) in edges
        addtograph!(graph, Symbol(left), Symbol(right))
        addtograph!(graph, Symbol(right), Symbol(left))
    end
    graph
end

partone = length

function shortestpaths(graph::Dict{Symbol,Vector{Symbol}})
    paths = Dict(keys(graph) .=> typemax(Int))
    queue = Set{Symbol}((:STT,))
    path = 0
    while !isempty(queue) && !isempty(graph)
        newqueue = Set{Symbol}()
        for here in queue
            if paths[here] > path
                paths[here] = path
                push!(newqueue, pop!(graph, here)...)
            end
        end
        queue = newqueue
        path += 1
    end
    paths
end

parttwo(paths) = count(<=(3), values(paths))
partthree(paths) = sum(values(paths))

if isinteractive()
    using REPL
    REPL.activate(Codyssi2024Day4)
elseif !isempty(ARGS)
    input = load(first(ARGS))
    println(partone(input))
    paths = shortestpaths(input)
    println(parttwo(paths))
    println(partthree(paths))
end

end
