def parse(io)
  enum = io
    .each_line
    .slice_after("\n")
  [
    enum.next.each_with_object([]) { |line, memo|
      line
        .chars
        .each_slice(4)
        .with_index
        .each { |part, i|
          memo[i] ||= []
          letter = part.join.gsub(/[^a-z]/i, '')
          memo[i].push(letter) unless letter.empty?
        }
    },
    enum.next.map { |line|
      count, from, to = line
        .match(/move (?<count>\d+) from (?<from>\d+) to (?<to>\d+)/)
        .named_captures
        .transform_keys(&:to_sym)
        .transform_values(&:to_i)
        .values_at(:count, :from, :to)
      { count: count, from: from - 1, to: to - 1 }
    }
  ]
end

if $PROGRAM_NAME == __FILE__
  state, commands = parse($stdin)
  commands.each do |command|
    count, from, to = command.values_at(:count, :from, :to)
    tmp = state[from].shift(count)
    state[to] = tmp.reverse + state[to]
  end
  puts state.map(&:first).join
end
