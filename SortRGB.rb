=begin
Given a string that consists of only 'R', 'G', and 'B' characters, and the only
operation that you can do are to swap two characters, how do you order the
string so that 'R' < 'G' < 'B' in linear time and constant space?

e.g.
"BGR" => "RGB"
"RGBBGR" => "RRGGBB"

The implementation here does this in place and in two phases. Each phase moves
two indices through the string. The first phase looks for the leftmost 'R' and
the leftmost non-'R', and then swaps them. The second phase picks up where the
first phase ended. If looks for the leftmost 'G' and leftmost non-'G', and then
swaps them. This is linear time and constant space.

The correctness is verified by enumerating permutations of "RGB" strings and
then comparing sorted strings against the expected sorted string generated
naively by counting the number of 'R', 'G', and 'B' characters and then
generating the sorted output.
=end

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
