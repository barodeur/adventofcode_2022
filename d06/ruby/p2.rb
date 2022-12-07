require_relative "./p1"

parse($stdin)
  .map(&method(:resolve).curry.call(14))
  .each(&method(:puts))

