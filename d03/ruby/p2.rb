require_relative "./p01.rb"

puts parse($stdin)
  .each_slice(3)
  .map { |group| group.map(&:chars).reduce(:intersection).first }
  .map(&method(:item_priority))
  .sum
