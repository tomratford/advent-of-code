using Printf
import Base.length

struct Range
    lower::Int
    upper::Int
end

function length(x::Range)
    return 1
end

function Base.iterate(x::Range)
    return x, 0
end

function Base.iterate(x::Range, state)
    return nothing
end

function within(x::Int, r::Range)
    return x >= r.lower && x <= r.upper
end

function Base.isless(x::Range, y::Range)
    return x.lower < y.lower
end

function parse_lines(input)
    lines = readlines(input)

    ranges = []
    available = []
    for x in lines
        if x == ""
            continue
        end
        if contains(x,"-")
            lower_upper = map(num -> parse(Int, num), split(x, "-"))
            append!(ranges, Range(lower_upper[1], lower_upper[2]))
        else
            append!(available, parse(Int, x))
        end
    end
    return (ranges, available)
end

function solve(input)
    part1 = 0
    (ranges, available) = input
    for a in available
        for r in ranges
            if within(a,r)
                part1 += 1
                break
            end
        end
    end

    # Part 2 fun!!!
    new_ranges = sort(ranges)
    #println(new_ranges)
    # Go through each range, compare back versus every range currently in,
    # prune top and bottom if there's overlap.
    part2 = 0
    lower = 0
    upper = 0 
    for (i,r) in enumerate(new_ranges)
        if i == 1
            lower = r.lower
            upper = r.upper
        else 
            # we know all the lowers are sorted now, 
            # so we just need to check if they overlap with the previous range or not
            if r.lower <= upper # overlaps with the prevous upper range, hence
                lower = upper + 1
                if r.upper <= lower # if completely encompassed by the prior
                    continue
                end
            else 
                lower = r.lower
            end
            upper = r.upper
        end

        #println(r," ", lower," ", upper, " ", upper - lower + 1)
        part2 += (upper - lower + 1)
    end

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