def addIntToString(i, s)
  final = Proc.new do |c|
    if c == 0 then "" else c.to_s end
  end
  s.split("").reduce(final) do |k, x|
      takeCarry = Proc.new do |c|
        y = (x.to_i % 10) + (c % 10)
        c2 = (c / 10) + (y / 10)
        x2 = (y % 10).to_s
        k.call(c2) + x2
      end
      takeCarry
    end.call(i)
end

puts(addIntToString(1, "199999999999999"))
puts(addIntToString(10000000, "0"))
