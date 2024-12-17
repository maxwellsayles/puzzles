// https://leetcode.com/problems/trapping-rain-water

// From left-to-right, track the highest height seen so far. Then repeat
// right-to-left. Then for each index, the amount of water it can hold is
// the min of the highest to the left and right minus the height of the index
// itself. Then sum all this up. O(n) time and O(n) memory.
fn trap(heights: &Vec<i32>) -> i32 {
    if heights.is_empty() {
	return 0;
    }

    let n = heights.len();

    let mut xs = vec![0; n];
    for i in 1..n {
	xs[i] = std::cmp::max(xs[i - 1], heights[i - 1]);
    }

    let mut ys = vec![0; n];
    for i in (0..n - 1).rev() {
	ys[i] = std::cmp::max(ys[i + 1], heights[i + 1]);
    }

    let mut zs = vec![0; n];
    for i in 0..n {
	let v = std::cmp::min(xs[i], ys[i]) as i32 - heights[i] as i32;
	zs[i] = std::cmp::max(0i32, v);
    }

    zs.iter().sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eg1() {
	let heights = vec![0,1,0,2,1,0,1,3,2,1,2,1];
	assert_eq!(trap(&heights), 6);
    }

    #[test]
    fn test_eg2() {
	let heights = vec![4,2,0,3,2,5];
	assert_eq!(trap(&heights), 9);
    }

    #[test]
    fn test_empty() {
	assert_eq!(trap(&Vec::new()), 0);
    }
}
