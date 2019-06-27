def sortRGB(xs)
  n = xs.length
  i, j = 0, 0
  while i < n && j < n do
    if xs[i] == 'R' then
      i += 1
    elsif j <= i || xs[j] != 'R' then
      j += 1
    else
      xs[i], xs[j] = xs[j], xs[i]
    end
  end

  i += 1 while i < n && xs[i] == 'R'
  j = i
  while i < n && j < n do
    if xs[i] == 'G' then
      i += 1
    elsif j <= i || xs[j] != 'G' then
      j += 1
    else
      xs[i], xs[j] = xs[j], xs[i]
    end
  end

  xs
end


puts sortRGB("RGB")
puts sortRGB("GRB")
puts sortRGB("GBR")
puts sortRGB("BGR")
puts sortRGB("BRG")
puts sortRGB("RBG")
puts sortRGB("RRGGBB")
puts sortRGB("GGRRBB")
puts sortRGB("GGBBRR")
puts sortRGB("BBGGRR")
puts sortRGB("BBRRGG")
puts sortRGB("RRBBGG")

