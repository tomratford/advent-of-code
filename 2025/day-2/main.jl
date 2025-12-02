using Printf

struct range
    lower::Int
    upper::Int
end

function parse_ranges(input)
    content = replace(read(input, String), "\n" => "")
    raw_ranges = split(content, ",")
    lower_upper = map(line -> map(num -> parse(Int, num), split(line, "-")), raw_ranges)
    return map(x -> range(x[1],x[2]), lower_upper)    
end

function solve(input)
    part1 = 0
    part2 = 0
    for rge in input
        for num in rge.lower:rge.upper
            power_n = trunc(Int, log10(num)) + 1
            if power_n%2==0 #easy case, first half should equal second half
                divider = 10^(power_n ÷ 2)
                first3 = num ÷ divider
                last3 = num - (first3 * divider)
                if first3 == last3
                    #println(num)
                    part1 += num
                    part2 += num
                    continue
                end
            end
            # work with a sensible number of primes to split by? 3, 5, 7
            if power_n%3==0
                nums = split3(num)
                if all(nums[1] .== nums)
                    part2 += num
                    continue
                end
            end
            if power_n%5==0
                nums = split5(num)
                if all(nums[1] .== nums)
                    part2 += num
                    continue
                end
            end
            if power_n%7==0
                divider = 10^(power_n ÷ 7)
                seventh7th = num % divider
                remaining7ths = num ÷ (10^((2*power_n) ÷ 7))
                sixth7th = (num ÷ divider) - (remaining7ths * divider)
                sths = split5(remaining7ths)
                if all(sths[1] .== sths) && sths[1] == sixth7th && sths[1] == seventh7th
                    part2 += num
                    continue
                end
            end
        end
    end
    @printf("Part 1: %d\nPart 2: %d\n", part1, part2)
end

function split3(num::Int)
    power_n = trunc(Int, log10(num)) + 1
    divider = 10^(power_n ÷ 3)
    third3rd = num % divider
    first3rd = num ÷ (10^((2*power_n) ÷ 3))
    second3rd = (num ÷ divider) - (first3rd * divider)
    return [first3rd, second3rd, third3rd]    
end

function split5(num::Int)
    power_n = trunc(Int, log10(num)) + 1
    divider = 10^(power_n ÷ 5)
    fifth5th = num % divider
    remaining5ths = num ÷ (10^((2*power_n) ÷ 5))
    fourth5th = (num ÷ divider) - (remaining5ths * divider)
    fths = split3(remaining5ths)
    return [fths[1], fths[2], fths[3], fourth5th, fifth5th]
end

function main()
    if length(ARGS) < 1
        error("Usage: julia main2.jl <input_file>")
    end
    input_file = ARGS[1]
    input_file |> parse_ranges |> solve
end

main()