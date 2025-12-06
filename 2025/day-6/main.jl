using Printf

struct range
    lower::Int
    upper::Int
end

function parse_input(input)
    content = readlines(input)
    sep_lines = map(line -> split(line), content)
    nums = map(line -> (map(word -> parse(Int, word), line)), sep_lines[1:end-1])
    nums2 = reduce(hcat, nums)
    ops = map(op -> ifelse(op == "*", *, +), sep_lines[end])
    return (nums2, ops)
end

function solve(input)
    part1 = 0
    (nums, ops) = input
    for (i, op) = enumerate(ops)
        part1 += reduce(op, nums[i,:])
    end
    @printf("Part 1: %d\n", part1)
end

function solvepart2(input)
    content = readlines(input)
    part2 = 0
    multtemp = 1
    addtemp = 0
    op = identity
    for (i,_) = enumerate(content[1])
        x = map(line -> line[i], content)
        if (all(c -> c == ' ', x))
            if op == *
                # println(multtemp)
                # println("==")
                part2 += multtemp
            elseif op == +
                # println(addtemp)
                # println("==")
                part2 += addtemp
            end
            multtemp = 1
            addtemp = 0
            op = identity
            continue
        end
        num = parse(Int, strip(reduce(*,x[1:end-1])))
        # println(num)
        if x[end] == '*'
            op = *
        end
        if x[end] == '+'
            op = +
        end
        if op == *
            multtemp *= num
        end
        if op == +
            addtemp += num
        end

        if i == length(content[1])
            if op == *
                # println(multtemp)
                # println("==")
                part2 += multtemp
            elseif op == +
                # println(addtemp)
                # println("==")
                part2 += addtemp
            end
        end
    end
    @printf("Part 2: %d\n", part2)
end

function main()
    if length(ARGS) < 1
        error("Usage: julia main2.jl <input_file>")
    end
    input_file = ARGS[1]
    input_file |> parse_input |> solve
    solvepart2(input_file)
end

main()