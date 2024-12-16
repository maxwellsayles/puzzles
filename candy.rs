// https://leetcode.com/problems/candy

// Iterating left-to-right, consider the minimum count from the left.
// Then iterating right-to-left, consider the minimum count from the right.
// (The above can be combined into a single pass).
// The minimum count is the maximum of the left and right approaches.
// Uses O(n) time and O(n) memory.
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

// Divide-and-conquer approach. `count` is mutable count of candy per child.
// Solve left half and right half separately, then from the center, first
// progress left and if the rating is higher than the child on the right, set
// the count to the right count + 1. The do the same thing progressing right.
// Uses O(nlogn) time and O(n) memory.
fn candy_dnc_worker(ratings: &[i32], count: &mut [i32]) {
    let n = ratings.len();
    if n <= 1 {
	return;
    }
    let m = n / 2;
    candy_dnc_worker(&ratings[..m], &mut count[..m]);
    candy_dnc_worker(&ratings[m..], &mut count[m..]);

    let mut i = m;
    while i < n	&& ratings[i] > ratings[i - 1] && count[i] <= count[i - 1] {
	count[i] = count[i - 1] + 1;
	i += 1;
    }

    let mut j = m - 1;
    while ratings[j] > ratings[j + 1] && count[j] <= count[j + 1] {
	count[j] = count[j + 1] + 1;
	if j == 0 {
	    break;
	}
	j -= 1;
    }
}

// Use the above worker that computes the candy per child and sum the counts.
fn candy_dnc(ratings: &Vec<i32>) -> i32 {
    let mut count = vec![1; ratings.len()];
    candy_dnc_worker(ratings, &mut count);
    count.iter().sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eg1() {
	let ratings = vec![1,0,2];
	assert_eq!(candy(&ratings), 5);
	assert_eq!(candy_dnc(&ratings), 5);
    }

    #[test]
    fn test_eg2() {
	let ratings = vec![1,2,2];
	assert_eq!(candy(&ratings), 4);
	assert_eq!(candy_dnc(&ratings), 4);
    }

    #[test]
    fn test_empty() {
	assert_eq!(candy(&Vec::new()), 0);
	assert_eq!(candy_dnc(&Vec::new()), 0);
    }

    #[test]
    fn test_single() {
	assert_eq!(candy(&vec![1]), 1);
	assert_eq!(candy_dnc(&vec![1]), 1);
    }
}
