require_relative "./p1"

puts parse($stdin)
  .count { |r1, r2| r1.cover?(r2.first) || r2.cover?(r1.first) } 

