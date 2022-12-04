puts $stdin
  .each_line
  .sum { |line|
    play, result = line.split(" ")
    play_idx = play.ord - "A".ord
    result_idx = result.ord - "Y".ord

    response_idx = (play_idx + result_idx + 3) % 3

    response_score = response_idx + 1
    result_score =
      case result_idx
      when -1 then 0
      when 0 then 3
      when 1 then 6
      end

    response_score + result_score
  }
