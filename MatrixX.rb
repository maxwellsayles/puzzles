# Given a 2d matrix, find the largest X in the matrix.
#
# For an n*n matrix, the solution here takes O(n^2) which is linear in the size
# of the input.
#
# It works in two parts.  First, for each X on the board, we associate a
# diagonal line and then update the end points of the line as we iterate
# over the board.  Then, for each X on the board, we compute the largest
# diagonal given the two lines that intersect that X.

require "test/unit"

class Line
  attr_accessor :x1, :y1, :x2, :y2
  def initialize(x, y)
    @x1 = x
    @y1 = y
    @x2 = x
    @y2 = y
  end
end

# Given two `Line`s, compute the largest X across their intersection.  It is
# assumed that the lines are diagonal and that they intersect at x, y.
# The integer representing the distance from the center is returned.
def computeX(l1, l2, x, y)
  res = [l1.x1, l1.x2, l2.x1, l2.x2]
  res = res.map { |v| (v - x).abs }
  res.min
end

def solve(board)
  n = board.length

  # For each X on the board, associate a diagonal line and update the end
  # points of the line as we go.
  l1 = {}
  l2 = {}
  (0..n-1).each do |j|
    (0..n-1).each do |i|
      if board[i][j] == 'x' then
        key = [i, j]
        
        l1[key] = Line.new(i, j)
        if i > 0 and j > 0 and l1[[i-1, j-1]] then
          l1[key] = l1[[i-1, j-1]]
          l1[key].x2 = i
          l1[key].y2 = j
        end
        
        l2[key] = Line.new(i, j)
        if i < n-1 and j > 0 and l2[[i+1, j-1]] then
          l2[key] = l2[[i+1, j-1]]
          l2[key].x1 = i
          l2[key].y1 = j
        end
      end
    end
  end

  # For each position on the board, we can intersect the diagonals and compute
  # the largest X intersecting that position
  res = []
  best = 1  # no X's with a radius smaller than 1
  (0..n-1).each do |j|
    (0..n-1).each do |i|
      key = [i, j]
      s = l1[key]
      t = l2[key]
      if s and t then
        size = computeX(s, t, i, j)
        rect = [i - size, j - size, i + size, j + size]
        if size > best then
          res = [rect]
          best = size
        elsif size == best then
          res << rect
        end
      end
    end
  end
  res
end

class TestBoards < Test::Unit::TestCase

  def test_none
    board = ['x']
    assert_equal([], solve(board))
    
    board = ['.']
    assert_equal([], solve(board))
  end

  def test_3x3
    board = ['...',
             '...',
             '...']
    assert_equal([], solve(board))
    
    board = ['x.x',
             '.x.',
             'x.x']
    assert_equal([[0, 0, 2, 2]], solve(board))
  end

  def test_5x5
    board = ['x....',
             '.x...',
             '..x..',
             '.....',
             '.....']
    assert_equal([], solve(board))

    board = ['x.x..',
             '.x...',
             'x.x..',
             '.....',
             '.....']
    assert_equal([[0, 0, 2, 2]], solve(board))

    board = ['x...x',
             '.x.x.',
             '..x..',
             '.x.x.',
             'x...x']
    assert_equal([[0, 0, 4, 4]], solve(board))

    board = ['x...x',
             '.x.x.',
             '..x..',
             '.x.x.',
             'x....']
    assert_equal([[1, 1, 3, 3]], solve(board))
  end

  def test_many
    board = ['x.xxx',
             '.xxx.',
             'xxx.x',
             'xx.x.',
             'x....']
    assert_equal([[0, 0, 2, 2],
                  [1, 1, 3, 3],
                  [0, 2, 2, 4]],
                 solve(board))
  end
 
end
