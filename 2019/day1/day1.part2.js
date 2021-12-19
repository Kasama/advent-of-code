const fs = require('fs')

const memory = {0: 1}
let sum = 0

const numbers = fs.readFileSync('day1.in')
  .toString()
  .split('\n')
  .map(n => parseInt(n, 10))

while(true) {
  numbers.forEach((n) => {
    sum += n
    if (memory[sum]) {
      console.log(sum)
      process.exit(0)
    } else {
      memory[sum] = 1
    }
  })

}
