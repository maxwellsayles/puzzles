=begin

Given a list of meeting start and end times, what's the minimal number of rooms
needed to schedule all meetings?

This implements two common approaches to this problem, the first is in O(n) time
and O(1) space, while the second is O(nlogn) time and O(logn) space.

Version 1:
First this splits the intervals into a list of start times and end times. Then
it tracks and index into both lists. If start <= end then increment start, else
increment end. At any given time instance, the indices represent the number of
meetings that have started and the number of meetings that have ended. The
difference is the number of meeting rooms needed.

Since we iterate over each list once, this is O(n) time and O(1) space.

Version 2:
Maintain a priority queue of meeting end times, and simply push meetings onto
the queue as they are processed. If a meetings start time > a previous meeting's
end time, pop it off of the queue. Track the max queue size.

=end

require 'fc'

def minMeetingRooms(times)
  return 0 if times.empty?
  starts, ends = times.transpose
  n = starts.length
  i, j = 0, 0
  maxRooms = 0
  while i < n do
    if starts[i] < ends[j] then
      i += 1
    elsif starts[i] == ends[j] then
      i += 1
      j += 1
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

def minMeetingRoomsPQ(times)
  q = FastContainers::PriorityQueue.new(:min)
  maxRooms = 0
  times.each do |t|
    s, e = t
    while !q.empty? && s >= q.top do
      q.pop
    end
    q.push(e, e)
    maxRooms = q.size if q.size > maxRooms
  end
  maxRooms
end

fail unless 0 == minMeetingRooms([])
fail unless 1 == minMeetingRooms([[1, 2]])
fail unless 1 == minMeetingRooms([[1, 2], [2, 3]])
fail unless 1 == minMeetingRooms([[1, 2], [2, 3], [3, 4]])
fail unless 1 == minMeetingRooms([[1, 2], [3, 4], [5, 6]])
fail unless 1 == minMeetingRooms([[1, 2], [2, 3], [3, 4]])
fail unless 2 == minMeetingRooms([[1, 2], [1, 2]])
fail unless 2 == minMeetingRooms([[1, 3], [2, 4], [3, 5]])
fail unless 3 == minMeetingRooms([[1, 2], [1, 2], [1, 2]])
fail unless 3 == minMeetingRooms([[1, 5], [2, 4], [3, 3]])

fail unless 0 == minMeetingRoomsPQ([])
fail unless 1 == minMeetingRoomsPQ([[1, 2]])
fail unless 1 == minMeetingRoomsPQ([[1, 2], [2, 3]])
fail unless 1 == minMeetingRoomsPQ([[1, 2], [2, 3], [3, 4]])
fail unless 1 == minMeetingRoomsPQ([[1, 2], [3, 4], [5, 6]])
fail unless 1 == minMeetingRoomsPQ([[1, 2], [2, 3], [3, 4]])
fail unless 2 == minMeetingRoomsPQ([[1, 2], [1, 2]])
fail unless 2 == minMeetingRoomsPQ([[1, 3], [2, 4], [3, 5]])
fail unless 3 == minMeetingRoomsPQ([[1, 2], [1, 2], [1, 2]])
fail unless 3 == minMeetingRoomsPQ([[1, 5], [2, 4], [3, 3]])


