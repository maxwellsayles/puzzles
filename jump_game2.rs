/**
 * Basically https://algo.monster/liteproblems/45
 * But if the goal isn't reachable, then return None.
 *
 * Solution is similar to the greedy approach in jump_game.rs
 * only we track both `j` the furthest point we can jump to, as well as
 * `k` the last jump point`. Whenever `i` == `k`, we increment the jump `cnt`.
 */

fn jump(nums: &Vec<i32>) -> Option<i32> {
    if nums.is_empty() {
	return Some(0);
    }

    let mut j = 0;
    let mut k = 0;
    let mut cnt = 0;
    let n = nums.len();
    for (i,x) in nums.iter().enumerate() {
	if i > j {
	    return None;
	}
	j = std::cmp::max(j, i + (*x as usize));
	if i == k && i < n - 1 {
	    cnt += 1;
	    k = j;
	}
    }
    Some(cnt)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eg1() {
	let nums: Vec<i32> = vec![2,3,1,1,4];
	assert_eq!(jump(&nums), Some(2));
    }

    #[test]
    fn test_eg2() {
	let nums: Vec<i32> = vec![2,3,0,1,4];
	assert_eq!(jump(&nums), Some(2));
    }

    #[test]
    fn test_empty() {
	assert_eq!(jump(&Vec::new()), Some(0));
    }

    #[test]
    fn test_one_jump() {
	assert_eq!(jump(&vec![1,0]), Some(1));
    }

    #[test]
    fn test_single() {
	assert_eq!(jump(&vec![0]), Some(0));
    }

    #[test]
    fn test_fail() {
	assert_eq!(jump(&vec![0,1]), None);
    }

    #[test]
    fn test_too_far() {
	assert_eq!(jump(&vec![3,0,0,0,0]), None);
    }
}
