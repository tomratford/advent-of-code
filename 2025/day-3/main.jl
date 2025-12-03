using Printf

function parse_lines(input)
    lines = readlines(input)
    digits = map(line -> split(line, ""), lines)
    return map(line -> map(digit -> parse(Int, digit), line), digits)
end

function solve(input)
    part1=0
    part2=0
    # part 1
    for line in input
        fst = maximum(line[1:end-1])
        lookafter = findfirst(x -> x == fst, line)
        snd = maximum(line[lookafter+1:end])
        max_num = 10*fst + snd
        #println(max_num)
        part1 += max_num
    end
    # part 2
    for line in input
        linesol = recur_solve_line(line, 12)
        #println(linesol)
        part2 += linesol
    end

    @printf("Part 1: %d\nPart 2: %d\n", part1, part2)
end

function recur_solve_line(vector, distance)
    if distance == 1
        return maximum(vector)
    else
        take = maximum(vector[1:end-distance+1])
    end
    #println("vector: ", vector, " distance: ", distance, " pickvector: ", vector[1:end-distance], " picked: ", take)
    lookafter = findfirst(x -> x == take, vector)
    return (take * (10^(distance-1))) + recur_solve_line(vector[lookafter+1:end], distance - 1)
end

function main()
    if length(ARGS) < 1
        error("Usage: julia main.jl <input_file>")
    end
    input_file = ARGS[1]
    input_file |> parse_lines |> solve
end

main()