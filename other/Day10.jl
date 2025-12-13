module Day10

using LinearAlgebra, JuMP
import HiGHS

struct Desc
  lights::BitVector
  buttons::BitMatrix
  joltages::Vector{Int}
end

function readbutton(s, n)
  vec::BitVector = BitVector(ntuple(x -> false, n))

  for elem = split(s[2:end-1], ',')
    i = parse(Int, elem)
    vec[i + 1] = true
  end

  vec'
end

function readdesc(s)
  parts = split(s, ' ')
  lights = convert(BitVector, map(==('#'), collect(parts[1][2:end-1])))
  n = length(lights)
  buttons = reduce(vcat, map(s -> readbutton(s, n), parts[2:end-1]))
  joltages = parse.(Int, split(parts[end][2:end-1], ','))
  Desc(lights, buttons, joltages)
end

function solve_part_2_once(d::Desc)
  A = d.buttons'
  b = d.joltages
  m, n = size(A)

  model = Model(HiGHS.Optimizer)
  set_silent(model)

  @variable(model, x[1:n], Int)
  @constraint(model, A * x == b)
  @constraint(model, x >= 0)
  @objective(model, Min, sum(x))

  optimize!(model)
  result = value.(x)
  return sum(result)
end

function solve_part_2(filename)
  input = read(filename, String)
  descs = readdesc.(split(input, '\n')[1:end-1])
  results = solve_part_2_once.(descs)
  return convert(Int, sum(results))
end

end
