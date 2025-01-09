/**
 * "Brace Expansion" represents a string with multiple choices. If a substring
 * has only one choice, it may appear as plain text. If the substring has
 * multiple choices, it is enclosed within curly braces, and each option is
 * separated by a comma. In this version of the problem, options can be
 * recursively nested as well. See the tests for examples.
 *
 * The solution here is in Rust, largely so I could gain more exposure to Rust.
 * The solution works by iterating over the string and maintaining a list of
 * "word prefixes" built so far, as well as a list of substrings built so far.
 * When an opening brace is encountered to solution recurses on a new list,
 * ending with a closing brace. When a comma is encountered the substring is
 * added to the list of words, and when the list is closed, the words are
 * appended to the prefixes.
 */

fn concat_word_list(w: &str, xs: &Vec<String>) -> Vec<String> {
    xs.iter().map(|x| w.to_string() + x).collect()
}

fn concat_list_list(xs: &Vec<String>, ys: &Vec<String>) -> Vec<String> {
    let mut res = Vec::new();
    for x in xs.iter() {
	res.append(&mut concat_word_list(x, ys));
    }
    res
}

fn take_word_set(input: &str) -> Vec<String> {
    let mut words = vec![String::new()];
    let mut rest = input;
    while !rest.is_empty() {
	let c = rest.chars().next().unwrap();
	if c.is_alphabetic() {
	    for word in words.iter_mut() {
		word.push(c);
	    }
	    rest = &rest[1..];
	} else if c == '{' {
	    let (xs, new_rest) = take_list(rest);
	    words = concat_list_list(&words, &xs);
	    rest = new_rest;
	} else {
	    panic!("Invalid input: {}", input);
	}

    }
    words
}

// Consumes all content between braces at the same depth.
fn take_list(input: &str) -> (Vec<String>, &str) {
    assert_eq!(input.chars().next(), Some('{'));

    let mut words: Vec<String> = Vec::new();
    let mut prefixes: Vec<String> = vec![String::new()];
    let mut rest = &input[1..];
    while !rest.is_empty() {
	let c = rest.chars().next().unwrap();
	if c == ',' {
	    words.append(&mut prefixes);
	    prefixes = vec![String::new()];
	    rest = &rest[1..];
	}
	if c == '}' {
	    words.append(&mut prefixes);
	    words.sort();
	    return (words, &rest[1..]);
	}
	if c == '{' {
	    let (xs, new_rest) = take_list(rest);
	    prefixes = concat_list_list(&prefixes, &xs);
	    rest = new_rest;
	}
	if c.is_alphabetic() {
	    for prefix in prefixes.iter_mut() {
		prefix.push(c);
	    }
	    rest = &rest[1..];
	}
    }

    panic!("Invalid input: {}", input);
}

#[cfg(test)]
mod tests_concat {
    use super::*;

    #[test]
    fn test_concat_word_many() {
	let mut ws = concat_word_list(
	    "asdf",
	    &vec!["qwer".to_string(), "zxcv".to_string()],
	);
	ws.sort();
	assert_eq!(
	    ws,
	    vec![
		"asdfqwer".to_string(),
		"asdfzxcv".to_string(),
	    ],
	);
    }

    #[test]
    fn test_concat_many_many() {
	let mut ws = concat_list_list(
	    &vec!["asdf".to_string(), "fdsa".to_string()],
	    &vec!["qwer".to_string(), "zxcv".to_string()],
	);
	ws.sort();
	assert_eq!(
	    ws,
	    vec![
		"asdfqwer".to_string(),
		"asdfzxcv".to_string(),
		"fdsaqwer".to_string(),
		"fdsazxcv".to_string(),
	    ],
	);
    }
}

#[cfg(test)]
mod tests_take_word_set {
    use super::*;

    #[test]
    fn test_empty() {
	let xs = take_word_set("");
	assert_eq!(xs, vec![String::new()]);
    }

    #[test]
    fn test_one_word() {
	let xs = take_word_set("asdf");
	assert_eq!(xs, vec!["asdf".to_string()]);
    }

    #[test]
    #[should_panic]
    fn test_csv() {
	take_word_set("asdf,qwer");
    }

    #[test]
    #[should_panic]
    fn test_symbol() {
	take_word_set(",");
    }

    #[test]
    fn test_char() {
	let xs = take_word_set("a");
	assert_eq!(xs, vec!["a".to_string()]);
    }

    #[test]
    #[should_panic]
    fn test_dangling() {
	take_word_set("a,");
    }

    #[test]
    fn test_embedded() {
	let xs = take_word_set("a{c,b}d");
	assert_eq!(xs, vec!["abd".to_string(), "acd".to_string()]);
    }

    #[test]
    fn test_nested() {
	let xs = take_word_set("a{c,{b,d}{g,f},e}{i,h}j");
	assert_eq!(xs, vec![
	    "abfhj",
	    "abfij",
	    "abghj",
	    "abgij",
	    "achj",
	    "acij",
	    "adfhj",
	    "adfij",
	    "adghj",
	    "adgij",
	    "aehj",
	    "aeij",
	].iter().map(|s| s.to_string()).collect::<Vec<String>>());
    }
}

#[cfg(test)]
mod tests_take_list {
    use super::*;

    #[test]
    #[should_panic]
    fn test_empty() {
	take_list("");
    }

    #[test]
    #[should_panic]
    fn test_unterminated() {
	take_list("c,a,b");
    }

    #[test]
    fn test_one() {
	let (ws, rest) = take_list("{a}");
	assert_eq!(ws, vec!["a".to_string()]);
	assert_eq!(rest, "");
    }

    #[test]
    fn test_two() {
	let (ws, rest) = take_list("{a,b}");
	assert_eq!(ws, vec!["a".to_string(), "b".to_string()]);
	assert_eq!(rest, "");
    }

    #[test]
    fn test_three() {
	let (ws, rest) = take_list("{c,a,b}");
	assert_eq!(ws, vec!["a".to_string(), "b".to_string(), "c".to_string()]);
	assert_eq!(rest, "");
    }

    #[test]
    fn test_nested1() {
	let (ws, rest) = take_list("{a,{b,c}}");
	assert_eq!(ws, vec!["a".to_string(), "b".to_string(), "c".to_string()]);
	assert_eq!(rest, "");
    }

    #[test]
    fn test_nested2() {
	let (ws, rest) = take_list("{a{b,c},d}");
	assert_eq!(ws, vec!["ab".to_string(), "ac".to_string(), "d".to_string()]);
	assert_eq!(rest, "");
    }

    #[test]
    fn test_nested3() {
	let (ws, rest) = take_list("{a{b,c}{d,e},f}");
	assert_eq!(ws, vec![
	    "abd".to_string(),
	    "abe".to_string(),
	    "acd".to_string(),
	    "ace".to_string(),
	    "f".to_string(),
	]);
	assert_eq!(rest, "");
    }
}
