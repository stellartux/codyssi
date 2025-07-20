module Codyssi2024Day1
# https://www.codyssi.com/view_problem_1

load(file::AbstractString) = parse.(Int, eachline(file))

partone = sum

parttwo(prices, vouchers=20) = sum(sort(prices, rev=true)[vouchers+1:end])

partthree(prices) =
    sum(price * factor for (price, factor) in zip(prices, Iterators.cycle((1, -1))))

if isinteractive()
    using REPL
    REPL.activate(Codyssi2024Day1)
elseif !isempty(ARGS)
    input = load(first(ARGS))
    println(partone(input))
    println(parttwo(input, occursin("example", ARGS[1]) ? 2 : 20))
    println(partthree(input))
end

end
