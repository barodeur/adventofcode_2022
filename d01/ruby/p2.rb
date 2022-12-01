puts $stdin
  .each_line
  .slice_after("\n")
  .map { |e| e.map(&:to_i).sum }
  .max(3)
  .sum

