module Day10

export Desc, readbutton, readjoltages, readdesc

using LinearAlgebra
using RowEchelon

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

# in principle the solution is then to compute the echelon form of the augmented matrix given by
# appending Desc.joltages or Desc.lights to transpose(Desc.buttons), and use a constraint solver
# to find the smallest possible assignment for the free variables

end
