#![warn(clippy::indexing_slicing)]

const C: [u32; 5] = [0; 5];

fn main() {
    #[allow(unconditional_panic, overflowing_literals, clippy::no_effect)]
    C[0xfffffe7fffffffffffffffffffffffff];
}
