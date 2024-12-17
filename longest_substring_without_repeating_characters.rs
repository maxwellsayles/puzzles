// https://leetcode.com/problems/longest-substring-without-repeating-characters

use std::collections::HashMap;

// This greedily uses the longest substring from the current character to the
// index where it was last seen (if at all). O(n) time and O(n) memory.
fn length_of_longest_substring(s: &str) -> usize {
    let mut res = 0;
    let mut hm = HashMap::new();
    let mut j = 0;
    for (i, c) in s.chars().enumerate() {
	match hm.insert(c, i) {
	    Some(k) => j = k + 1,
	    None => {},
	}
	res = std::cmp::max(res, 1 + i - j);
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eg1() {
	assert_eq!(length_of_longest_substring("abcabcbb"), 3);
    }

    #[test]
    fn test_eg2() {
	assert_eq!(length_of_longest_substring("bbbbb"), 1);
    }

    #[test]
    fn test_eg3() {
	assert_eq!(length_of_longest_substring("pwwkew"), 3);
    }

    #[test]
    fn test_empty() {
	assert_eq!(length_of_longest_substring(""), 0);
    }

    #[test]
    fn test_single() {
	assert_eq!(length_of_longest_substring("a"), 1);
    }

    #[test]
    fn test_full_string() {
	assert_eq!(length_of_longest_substring("abcxyz"), 6);
    }

    #[test]
    fn test_barbell() {
	assert_eq!(length_of_longest_substring("xabcdefgx"), 8);
    }
}
