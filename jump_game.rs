// https://leetcode.com/problems/jump-game/

// O(n) runtime greedy. For each i that can be reached, update the furthest
// index that can be reached.
fn can_jump_greedy(nums: &Vec<i32>) -> bool {
    let mut r = 0;
    let mut i = 0;
    while i < nums.len() && i <= r {
	r = std::cmp::max(i + nums[i] as usize, r);
	i += 1;
    }
    i == nums.len()
}

// O(n^2) runtime DP solution. O[i] is true if any of O[i+1]..=O[j] is true
// where j = i + nums[i].
fn can_jump_dp(nums: &Vec<i32>) -> bool {
    if nums.is_empty() {
	return true;
    }
    let n = nums.len();
    let mut cj = vec![false; n];
    cj[n - 1] = true;
    for i in (0 .. n - 1).rev() {
	let j = i + nums[i] as usize;
	cj[i] = ((i+1)..=j).any(|idx| cj[idx]);
    }
    cj[0]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example1() {
	let nums: Vec<i32> = vec![2,3,1,1,4];
	assert!(can_jump_greedy(&nums));
	assert!(can_jump_dp(&nums));
    }

    #[test]
    fn test_example2() {
	let nums: Vec<i32> = vec![3,2,1,0,4];
	assert!(!can_jump_greedy(&nums));
	assert!(!can_jump_dp(&nums));
    }

    #[test]
    fn test_empty() {
	assert!(can_jump_greedy(&Vec::new()));
	assert!(can_jump_dp(&Vec::new()));
    }

    #[test]
    fn test_single_zero() {
	assert!(can_jump_greedy(&vec![0]));
	assert!(can_jump_dp(&vec![0]));
    }

    #[test]
    fn test_fail() {
	let nums: Vec<i32> = vec![0, 0];
	assert!(!can_jump_greedy(&nums));
	assert!(!can_jump_dp(&nums));
    }

    #[test]
    fn test_long_jump() {
	let nums: Vec<i32> = vec![10,1,0,0,0];
	assert!(can_jump_greedy(&nums));
	assert!(can_jump_dp(&nums));
    }
}
