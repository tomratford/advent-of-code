const fs = require("fs")
const os = require("os")

// parse
parse = fs.readFileSync(process.argv[2], "utf-8")
    .split(os.EOL)

// part1
const p1 = {
    "A Y": 7, "B Z": 8, "C X": 9,
    "A X": 4, "B X": 5, "C Y": 6,
    "A Z": 1, "B Y": 2, "C Z": 3
}

console.log(parse.reduce((a, v) => a + p1[v], 0))

// part2
const p2 = {
    "A Y": 4, "B Z": 9, "C X": 2,
    "A X": 3, "B X": 1, "C Y": 6,
    "A Z": 8, "B Y": 5, "C Z": 7
}

console.log(parse.reduce((a, v) => a + p2[v], 0))
