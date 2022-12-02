const fs = require("fs")
const os = require("os")

// parse
parse = fs.readFileSync(process.argv[2], "utf-8")
    .split(os.EOL)

// part1

const win = ["A Y", "B Z", "C X"]
const draw = ["A X", "B Y", "C Z"]

console.log(parse.reduce((a, v) => {
    score = 0
    if (win.includes(v)) {
        score += 6
    } else if (draw.includes(v)) {
        score += 3
    }

    switch (v.split(" ")[1]) {
        case 'X':
            score += 1
            break
        case 'Y':
            score += 2
            break
        case 'Z':
            score += 3
            break
    }

    return a + score
}, 0))

// part2
console.log(parse.reduce((a, v) => {
    score = 0

    tmp = v.split(" ")
    opp = tmp[0]
    res = tmp[1]

    switch (res) {
        case 'X':
            switch (opp) {
                case 'A':
                    score += 3
                    break
                case 'B':
                    score += 1
                    break
                case 'C':
                    score += 2
                    break;
            }
            break
        case 'Y':
            score += 3
            switch (opp) {
                case 'A':
                    score += 1
                    break
                case 'B':
                    score += 2
                    break
                case 'C':
                    score += 3
                    break;
            }
            break
        case 'Z':
            score += 6
            switch (opp) {
                case 'A':
                    score += 2
                    break
                case 'B':
                    score += 3
                    break
                case 'C':
                    score += 1
                    break;
            }
            break
    }

    return a + score
}, 0))
