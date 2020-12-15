function run(numbers, limit::Int64)
  vec = zeros(Int32, 30000000)

  turn = 1
  for num in numbers[1:end-1]
    vec[num + 1] = turn
    turn += 1
  end

  last::Int64 = numbers[end]

  while turn < limit
    idx       = last + 1 # Julia's arrays are one based
    prev_turn = vec[idx]
    vec[idx]  = turn
    last      = (prev_turn > 0) ? turn - prev_turn : 0
    turn     += 1
  end

  last
end

@show run([12,20,0,6,1,17,7], 2020)
@show run([12,20,0,6,1,17,7], 30000000)
@time run([12,20,0,6,1,17,7], 30000000)
