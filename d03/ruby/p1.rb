def parse(io)
  io
    .each_line
    .map(&:strip)
end

def halves(arr)
  arr.each_slice((arr.size/2.0).round).to_a
end

def item_priority(item)
  (item.ord > "Z".ord ? (item.ord - "a".ord) : (item.ord - 'A'.ord + 26)) + 1
end

if $PROGRAM_NAME == __FILE__
  puts parse($stdin)
    .map(&:chars)
    .map(&method(:halves))
    .map { |left, right| left.intersection(right).first }
    .map(&method(:item_priority))
    .sum
end
