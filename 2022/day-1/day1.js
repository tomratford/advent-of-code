const fs = require("fs")

// parse
nums = fs.readFileSync(process.argv[2], "utf-8")
    .split("\n")
    .map((x) => parseInt(x))

// part1
arr = [0]
for (i = 0; i < nums.length; i++) {
    if (isNaN(nums[i])) {
        arr.push(0)
    } else {
        arr[arr.length - 1] += nums[i]
    }
}
console.log(arr.reduce((a, b) => Math.max(a, b), -Infinity))

// part2
arr.sort((a, b) => b - a)
top3 = (arr.slice(0, 3))
console.log(top3.reduce((a, v) => a + v))