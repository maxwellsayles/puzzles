=begin

Given a list of meeting start and end times, what's the minimal number of rooms
needed to schedule all meetings?

First this splits the intervals into a list of start times and end times. Then
it tracks and index into both lists. If start <= end then increment start, else
increment end. At any given time instance, the indices represent the number of
meetings that have started and the number of meetings that have ended. The
difference is the number of meeting rooms needed.

Since we iterate over each list once, this is O(n) time and O(1) space.

=end


def minMeetingRooms(times)
  return 0 if times.empty?
  starts, ends = times.transpose
  n = starts.length
  i, j = 0, 0
  maxRooms = 0
  while i < n do
    if starts[i] <= ends[j] then
      i += 1
    else
      j += 1
    end
    cnt = i - j
    maxRooms = cnt if cnt > maxRooms
  end
  cnt = i - j
  maxRooms = cnt if cnt > maxRooms
  maxRooms
end

fail unless 0 == minMeetingRooms([])
fail unless 1 == minMeetingRooms([[1, 2], [3, 4], [5, 6]])
fail unless 2 == minMeetingRooms([[1, 2], [2, 3], [3, 4]])
fail unless 3 == minMeetingRooms([[1, 5], [2, 4], [3, 3]])
fail unless 3 == minMeetingRooms([[1, 3], [2, 4], [3, 5]])
