#![allow(unused)]
#![warn(clippy::map_or_eq)]

fn potential_side_effect<T>() -> T {
    panic!();
}

fn main() {
    let opt = None::<i32>;
    let _ = opt.map_or(false, |val| val == 5); // lint
    let _ = potential_side_effect::<Option<i32>>().map_or(false, |val| val == 5); // lint
    let _ = opt.map_or(false, |val| val != potential_side_effect()); // don't lint
}
