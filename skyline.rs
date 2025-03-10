// See Skyline.py for a more detailed description.

use std::collections::{ BinaryHeap, BTreeSet };

#[derive(Debug, Eq, PartialEq)]
struct Extent {
    l: i32,
    r: i32,
    h: i32,
}

impl Extent {
    fn new(l: i32, r: i32, h: i32) -> Self {
	Self { l, r, h }
    }
}

impl Ord for Extent {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
	if self.h == other.h {
	    if self.l == other.l {
		return self.r.cmp(&other.r);
	    }
	    return self.l.cmp(&other.l);
	}
	return self.h.cmp(&other.h);
    }
}

impl PartialOrd for Extent {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn lhr_cmp(this: &Extent, that: &Extent) -> std::cmp::Ordering {
    if this.l == that.l {
	if this.h == that.h {
	    return this.r.cmp(&that.r);
	}
	return this.h.cmp(&that.h);
    }
    return this.l.cmp(&that.l);
}

fn skyline(buildings: &Vec<(i32, i32, i32)>) -> Vec<(i32, i32)> {
    if buildings.is_empty() {
	return Vec::new();
    }

    let mut extents = buildings
	.iter()
	.map(|b| Extent::new(b.0, b.1, b.2)).collect::<Vec<Extent>>();
    extents.sort_by(lhr_cmp);

    let mut points_set = BTreeSet::new();
    for e in extents.iter() {
	points_set.insert(e.l);
	points_set.insert(e.r);
    }

    let mut q = BinaryHeap::new();
    let base_extent = Extent::new(
	*points_set.first().unwrap(),
	*points_set.last().unwrap(),
	0,
    );
    q.push(&base_extent);

    let mut res: Vec<(i32, i32)> = Vec::new();
    let mut i = 0;
    for p in points_set.iter() {
	while i < extents.len() && extents[i].l <= *p {
	    q.push(&extents[i]);
	    i += 1;
	}
	while !q.is_empty() {
	    if let Some(e) = q.pop() {
		if e.r > *p {
		    if res.is_empty() || res.last().unwrap().1 != e.h {
			res.push((*p, e.h));
		    }
		    q.push(e);
		    break;
		}
	    }
	}
    }
    res.push((*points_set.last().unwrap(), 0));
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
	let buildings = vec![(2,9,10),(3,7,15),(5,12,12),(15,20,10),(19,24,8)];
	let expected_output = vec![(2,10),(3,15),(7,12),(12,0),(15,10),(20,8),(24,0)];
	let actual_output = skyline(&buildings);
	assert_eq!(expected_output, actual_output);
    }

    #[test]
    fn test2() {
	let buildings = vec![(0,2,3),(2,5,3)];
	let expected_output = vec![(0,3),(5,0)];
	let actual_output = skyline(&buildings);
	assert_eq!(expected_output, actual_output);
    }

    #[test]
    fn test_empty() {
	assert_eq!(skyline(&Vec::new()), Vec::new());
    }

    #[test]
    fn test_single() {
	assert_eq!(skyline(&vec![(1,2,3)]), vec![(1,3),(2,0)]);
    }

    #[test]
    fn test_contained() {
	let buildings = vec![(1,10,5),(3,6,3)];
	let output = vec![(1,5),(10,0)];
	assert_eq!(skyline(&buildings), output);
    }
}
