using Printf

struct Coord
    x::Int
    y::Int
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
    for (k,c) in input
        if c == '.'
            continue
        else
            adjs = get_adj(k)
            x = map(p -> get(input, p, '.'), adjs)
            if count(x .== '@') < 4
                part1 += 1
            end
        end
    end
    part2 = solvepart2(input)
    @printf("Part 1: %d\nPart 2: %d\n", part1, part2)
end

function solvepart2(input::Dict{Coord, Char})
    score = 0
    input_copy = deepcopy(input)
    for (k,c) in input
        if c == '.'
            continue
        else
            adjs = get_adj(k)
            x = map(p -> get(input, p, '.'), adjs)
            if count(x .== '@') < 4
                input_copy[k] = '.'
                score += 1
            end
        end
    end
    if input_copy != input
        return score + solvepart2(input_copy)
    else
        return score
    end
end

function main()
    if length(ARGS) < 1
        error("Usage: julia main.jl <input_file>")
    end
    input_file = ARGS[1]
    input_file |> parse_lines |> solve
end

main()