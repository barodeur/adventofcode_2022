puts $stdin
  .each_line
  .sum { |line|
    play, response = line.split(" ")
    play_idx = play.ord - "A".ord
    response_idx = response.ord - "X".ord
    result = (play_idx - response_idx + 3) % 3

    response_score = response_idx + 1
    result_score =
      case result
      when 0 then 3
      when 1 then 0
      when 2 then 6
      end

    response_score + result_score
  }
