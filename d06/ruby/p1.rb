require "set"

def parse(io)
  io
    .each_char
    .slice_after("\n")
end

def all_unique?(arr)
  arr.to_set.length == arr.length
end

def resolve(marker_size, chars)
  marker_size + chars
    .each_cons(marker_size)
    .find_index(&method(:all_unique?))
end

if $PROGRAM_NAME == __FILE__
  parse($stdin)
    .map(&method(:resolve).curry.call(4))
    .each(&method(:puts))
end
