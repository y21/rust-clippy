#![allow(unused)]

static SOME_STATIC: String = String::new();

static UNIT: () = compute(&SOME_STATIC);

pub const fn compute<T>(_: T)
where
    T: Copy,
{
}

fn main() {}
