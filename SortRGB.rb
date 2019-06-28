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

def ith(i)
  return '' if i == 0
  i -= 1
  s = ''
  while i > 2 do
    s += 'RGB'[i % 3]
    i = (i - 3) / 3
  end
  s += 'RGB'[i % 3]
  s
end

def validate(i)
  s = ith(i)
  r = s.count('R')
  g = s.count('G')
  b = s.count('B')
  expected = 'R' * r + 'G' * g + 'B' * b
  fail unless sortRGB(s) == expected
end

(0..100000).each do |x| validate(x) end
puts "All tests pass"
