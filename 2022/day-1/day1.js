const fs = require("fs")
const os = require("os")

// parse
nums = fs.readFileSync(process.argv[2], "utf-8")
    .split(os.EOL + os.EOL)
    .map((x) =>
        x.split(os.EOL)
            .map((y) => parseInt(y))
            .filter((v) => !isNaN(v))
            .reduce((a, v) => a + v)

    )
    .sort((a, b) => b - a)

// part1
console.log(nums[0])

// part2
top3 = (nums.slice(0, 3))
console.log(top3.reduce((a, v) => a + v))