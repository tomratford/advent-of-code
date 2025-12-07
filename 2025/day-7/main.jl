using Printf

struct Coord
    x::Int
    y::Int
end

function Base.:+(s::Coord, a::Coord)
    return Coord(s.x + a.x, s.y + a.y)
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

function get_adj(p::Coord)
    offsets = [(di, dj) for di in -1:1, dj in -1:1 if !(di == 0 && dj == 0)]
    return [Coord(p.x+i,p.y+j) for (i,j) in offsets]
end

function parse_lines(input)
    data = Dict{Coord, Char}()
    for (j, line) in enumerate(readlines(input))
        for (i,ch) in enumerate(line)
            data[Coord(i,j)] = ch
        end
    end
    return data
end

function solve(input::Dict{Coord, Char})
    part1 = 0
    beams = Coord[]
    start_x = 0
    visited = Dict{Coord, Int}()
    for (k,v) in input
        if v == 'S'
            start_x = k.x
            append!(beams, k)
        end
    end

    while !isempty(beams)
        beam = pop!(beams)
        new_beam = Coord(beam.x, beam.y + 1)
        if !haskey(input, new_beam) || haskey(visited, new_beam)
            continue
        elseif input[new_beam] == '^'
            part1 += 1
            append!(beams, [Coord(beam.x - 1, beam.y + 1), Coord(beam.x + 1, beam.y + 1)])
            visited[new_beam] = 0
        else
            append!(beams, new_beam)
        end
    end
    
    lowest_x = minimum([a.x for (a,_) in input])
    biggest_x = maximum([a.x for (a,_) in input])

    lowest_y = minimum([a.y for (a,_) in input])
    biggest_y = maximum([a.y for (a,_) in input])

    xs = lowest_x+1:biggest_x+1
    ys = lowest_y+1:biggest_y+1

    scores = zeros(length(xs))
    scores[start_x] = 1
    for j in ys
        for i in xs
            c = Coord(i,j)
            if haskey(input, c)
                if input[c] == '^'
                    scores[c.x - 1] += scores[c.x]
                    scores[c.x + 1] += scores[c.x]
                    scores[c.x] = 0
                end
            end
        end
    end
    part2 = sum(scores)

    @printf("Part 1: %d\nPart 2: %d\n", part1, part2)
end

function main()
    if length(ARGS) < 1
        error("Usage: julia main.jl <input_file>")
    end
    input_file = ARGS[1]
    input_file |> parse_lines |> solve
end

main()