def parse(io)
  io
    .each_line
    .slice_after("\n")
    .map { |e| e.map(&:to_i).sum }
end

if $PROGRAM_NAME == __FILE__
  puts parse($stdin).max
end

