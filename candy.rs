// https://leetcode.com/problems/candy

// Iterating left-to-right, consider the minimum count from the left.
// Then iterating right-to-left, consider the minimum count from the right.
// (The above can be combined into a single pass).
// The minimum count is the maximum of the left and right approaches.
fn candy(ratings: &Vec<i32>) -> i32 {
    if ratings.is_empty() {
	return 0;
    }
    let n = ratings.len();
    let mut xs = vec![0; n];
    let mut ys = vec![0; n];
    xs[0] = 1;
    ys[n-1] = 1;
    for i in 0..n-1 {
	let x = i + 1;
	let y = n - 2 - i;
	xs[x] = if ratings[x] > ratings[x - 1] { xs[x - 1] + 1} else { 1 };
	ys[y] = if ratings[y] > ratings[y + 1] { ys[y + 1] + 1} else { 1 };
    }
    std::iter::zip(xs, ys).map(|(x, y)| std::cmp::max(x, y)).sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eg1() {
	let ratings = vec![1,0,2];
	assert_eq!(candy(&ratings), 5);
    }

    #[test]
    fn test_eg2() {
	let ratings = vec![1,2,2];
	assert_eq!(candy(&ratings), 4);
    }

    #[test]
    fn test_empty() {
	assert_eq!(candy(&Vec::new()), 0);
    }

    #[test]
    fn test_single() {
	assert_eq!(candy(&vec![1]), 1);
    }
}
