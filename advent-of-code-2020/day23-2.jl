# Port of Jonathan Chan's Racket code to Julia:
# https://github.com/ionathanch/adventofcode-2020/blob/main/src/23.rkt

function getcups(input::Array{Int32,1}, n)::Array{Int32,1}
  cups = Array{Int32}(undef, n)
  len  = length(input)

  for i = 1:len-1
    cups[input[i]] = input[i+1]
  end

  cups[input[end]] = len+1

  for i = len+1:n-1
    cups[i] = i+1
  end

  cups[n] = input[1]
  cups
end

prev(idx) = idx == 1 ? 1000000 : idx - 1

function play(cups, current)
  one   = cups[current]
  two   = cups[one]
  three = cups[two]
  four  = cups[three]

  dest = prev(current)
  while dest == one || dest == two || dest == three
    dest = prev(dest)
  end

  next          = cups[dest]
  cups[current] = four
  cups[three]   = next
  cups[dest]    = one

  cups[current]
end

function part2(input::Array{Int32,1})
  cups    = getcups(input, 1000000)
  current = input[1]
  moves   = 0

  for _ = 1:10000000
    current = play(cups, current)
  end

  one = convert(Int64, cups[1])
  two = convert(Int64, cups[one])

  one * two
end

@time part2(convert(Array{Int32,1}, [4, 9, 6, 1, 3, 8, 5, 2, 7]))
