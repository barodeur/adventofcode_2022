def parse(io)
  io
    .each_line
    .map { |line|
      line
        .split(",")
        .map { |part| part.split("-").map(&:to_i) }
        .map { |s, e| Range.new(s, e) }
    }
end

if $PROGRAM_NAME == __FILE__
  puts parse($stdin)
    .count { |r1, r2| r1.cover?(r2) || r2.cover?(r1) } 
end

