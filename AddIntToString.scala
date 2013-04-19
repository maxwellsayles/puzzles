
def addIntToString(i: Int, s: String): String = {
  def last(carry: Int): String = {
    if (carry == 0) ""
    else carry.toString
  }
  def step(f: Int => String, i: Int)(x: Int): String = {
    val y = (x % 10) + i
    f((y / 10) + (x / 10)) + (y % 10).toString
  }
  s.toList.map(_.toInt - '0').foldLeft(last _)(step)(i)
}

println(addIntToString(1, "1999999999999999999999999999999"))
println(addIntToString(3, "0000000000000000009999999999999"))
println(addIntToString(3, "0000000000000000000000000000000"))
println(addIntToString(123456789, "0"))
