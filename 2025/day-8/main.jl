using Printf

struct Coord
    x::Int
    y::Int
    z::Int
end

function Base.:+(a::Coord, b::Coord)
    return Coord(a.x + b.x, a.y + b.y, a.z + b.z)
end

function Base.:-(a::Coord, b::Coord)
    return Coord(a.x - b.x, a.y - b.y, a.z - b.z)
end

function Base.:^(a::Coord, b)
    return Coord(a.x^b, a.y^b, a.z^b)
end

Base.:zero(x::Coord) = Coord(0,0,0)

Base.:-(a::Coord) = Coord(-a.x, -a.y, -a.z)

function Base.:sum(a::Coord)
    return a.x + a.y + a.z
end

function Base.length(x::Coord)
    return 1
end

function Base.iterate(x::Coord)
    return x, 0
end

function Base.iterate(x::Coord, state)
    return nothing
end

function Base.iterate(x::Tuple{Coord,Coord})
    return x, 0
end

function Base.iterate(x::Tuple{Coord,Coord}, state)
    return nothing
end

Base.zero(x::Type{Tuple{Coord, Coord}}) = (Coord(0,0,0), Coord(0,0,0))

function euclid(a::Coord, b:: Coord)
    return sqrt(sum((a - b)^2))
end

function parse_lines(input)
    data = Coord[]
    for (_, line) in enumerate(readlines(input))
        nums = map(x -> parse(Int, x), split(line, ","))
        append!(data, Coord(nums[1], nums[2], nums[3]))
    end
    return data
end

function solve(input)
    ## Work out connections (the graph), traverse the resulting graph
    distances = Dict{Coord, Dict{Coord, Float64}}()
    for (i,c) in enumerate(input)
        for (j, k) in enumerate(input)
            if i == j
                continue
            end
            if !haskey(distances,c)
                distances[c] = Dict{Coord, Float64}()
            end
            distances[c][k] = euclid(c,k)
        end
    end
    
    tenshortest = zeros(Tuple{Coord,Coord}, 10) 
    for n = 1:10
        smallestdistances = Dict{Coord, Tuple{Float64, Coord}}()
        for (i,c) in distances
            smallestdistances[i] = findmin(c)
        end

        smallestcoord = Tuple{Coord, Coord}
        smallestval = 9999999999.9
        for (k,value) in smallestdistances
            (v,c) = value
            if v < smallestval
                smallestcoord = (k, c)
                smallestval = v
            end
        end

        (k,c) = smallestcoord

        println(smallestval, ",(", k,",", c,")")

        distances[k][c] = 999999999
        distances[c][k] = 999999999
        tenshortest[n] = smallestcoord
    end

    visited = Vector{Vector{Coord}}(undef, 0)
    for (c,k) in tenshortest
        if isempty(visited)
            visited = push!(visited, [c,k])
            continue
        end
        for (i,l) in enumerate(visited)
            if c ∈ l && k ∈ l
                break
            elseif c ∈ l && k ∉ l
                append!(l, k)
            elseif c ∉ l && k ∈ l
                append!(l,c)
            elseif i == length(visited)
                push!(visited, [c, k])
            end
        end
    end
    map(println, visited)
end

function main()
    if length(ARGS) < 1
        error("Usage: julia main.jl <input_file>")
    end
    input_file = ARGS[1]
    input_file |> parse_lines |> solve
end

main()