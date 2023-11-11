#![warn(clippy::covariant_mutable_types)]

use std::ptr::NonNull;

fn main() {
    {
        #[derive(Copy, Clone)]
        struct Ptr<T>(NonNull<T>);
        impl<T> Ptr<T> {
            pub fn as_mut(&mut self) -> &mut T {
                unsafe { self.0.as_mut() }
            }
        }
    }
}
