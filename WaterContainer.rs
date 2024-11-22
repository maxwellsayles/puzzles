// https://leetcode.com/problems/container-with-most-water/description/

// For each i<j compute the area of water and return the maximum.
fn water_container_brute(heights: &Vec<u32>) -> u32 {
    let n = heights.len();
    let mut max_area = 0;
    for j in 1..n {
	let y = heights[j];
	for i in 0..j {
	    let x = heights[i];
	    let h = std::cmp::min(x, y);
	    let area = h * ((j - i) as u32);
	    max_area = std::cmp::max(max_area, area);
	}
    }
    max_area
}

// For any k, i < k < j, where h[k] <= h[i], the area between [k,j] is no
// larger than the area between [i,j]. As such, we can skip every such k.
// This works by iterating i and j from the outside in moving the index that
// points to the lower height.
fn water_container_greedy(heights: &Vec<u32>) -> u32 {
    let mut i = 0;
    let mut j = heights.len() - 1;
    let mut max_area = 0;
    while i < j {
	let x = heights[i];
	let y = heights[j];
	let h = std::cmp::min(x, y);
	let area = h * ((j - i) as u32);
	max_area = std::cmp::max(max_area, area);
	if x < y {
	    i += 1;
	} else {
	    j -= 1;
	}
    }
    max_area
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_leetcode_example() {
	let heights = vec![1, 8, 6, 2, 5, 4, 8, 3, 7];

	let area_brute = water_container_brute(&heights);
	assert_eq!(area_brute, 49);

	let area_greedy = water_container_greedy(&heights);
	assert_eq!(area_greedy, 49);
    }
}
