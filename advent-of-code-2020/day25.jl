# Port of my Racket solution:
# see: https://github.com/lojic/LearningRacket/blob/master/advent-of-code-2020/day25.rkt

const input   = [ 3248366, 4738476 ]
const divisor = 20201227
const subject = 7

transform1(subject, val) = (val * subject) % divisor

function transform(subject, n, val=1)
  for _ = 1:n
    val = transform1(subject, val)
  end
  val
end

function findloopsize(subject, key)
  val, n = 1, 0
  while val !== key
    val = transform1(subject, val)
    n += 1
  end
  n
end

encryptionkey(key1, key2) = transform(key2, findloopsize(subject, key1))

@time encryptionkey(input...)

using Test
@test transform(subject, 8) === 5764801
@test transform(subject, 11) === 17807724
@test findloopsize(subject, 5764801) === 8
@test findloopsize(subject, 17807724) === 11
@test encryptionkey(5764801, 17807724) === 14897079
@test encryptionkey(17807724, 5764801) === 14897079
@test encryptionkey(input...) === 18293391 # Part 1
