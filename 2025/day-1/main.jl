using Printf

struct Instruction
    direction::Char
    distance::Int
end

function parse_instructions(input)
    lines = filter(line -> !isempty(strip(line)), input)
    return map(line -> Instruction(first(line), parse(Int, line[2:end])), lines)
end

function solve(instructions)
    dial = 50
    part1 = 0
    part2 = 0
    for inst in instructions
        for _ in 1:inst.distance
            if inst.direction == 'L'
                dial -= 1
            elseif inst.direction == 'R'
                dial += 1
            end
            dial = mod(dial, 100)
            if dial == 0
                part2 += 1
            end
        end
        if dial == 0
            part1 += 1
        end
    end
    @printf("Part 1: %d\nPart 2: %d\n", part1, part2)
end

function main()
    if length(ARGS) < 1
        error("Usage: julia main.jl <input_file>")
    end
    input_file = ARGS[1]
    input = readlines(input_file)
    input |> parse_instructions |> solve
end

main()