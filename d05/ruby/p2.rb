require_relative "./p1"

state, commands = parse($stdin)
commands.each do |command|
  count, from, to = command.values_at(:count, :from, :to)
  tmp = state[from].shift(count)
  state[to] = tmp + state[to]
end
puts state.map(&:first).join
