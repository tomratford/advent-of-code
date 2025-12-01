using Printf

struct Dial
    pos::Int
    part1::Int
    part2::Int
end

function Base.:+(a::Dial, b::Int)
    pos = a.pos
    part1 = a.part1
    part2 = a.part2
    for _ in 1:abs(b)
        pos += sign(b)
        pos = mod(pos,100)
        if pos == 0
            part2 += 1
        end
    end
    if pos == 0
        part1 += 1
    end

    return Dial(
        pos,
        part1,
        part2
    )
end

function solve(instructions)
    dial = Dial(50, 0, 0)
    for i in instructions
        dial = dial + i
    end
    @printf("Part 1: %d\nPart 2: %d\n", dial.part1, dial.part2)
end

function parse_instructions(input)
    lines = filter(line -> !isempty(strip(line)), input)
    return map(line -> ifelse(first(line) == 'L', -1 * parse(Int, line[2:end]), parse(Int, line[2:end])), lines)
end

function main()
    if length(ARGS) < 1
        error("Usage: julia main2.jl <input_file>")
    end
    input_file = ARGS[1]
    input = readlines(input_file)
    input |> parse_instructions |> solve
end

main()