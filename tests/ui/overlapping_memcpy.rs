#![allow(unused)]
#![warn(clippy::overlapping_memcpy)]

fn main() {
    let mut s = [0; 100];
    let ptr = s.as_mut_ptr();
    unsafe {
        std::ptr::copy_nonoverlapping(ptr, ptr, 10);
        std::ptr::copy_nonoverlapping(ptr, ptr.add(9), 10);
        std::ptr::copy_nonoverlapping(ptr, ptr.add(10), 10);
        std::ptr::copy_nonoverlapping(ptr.add(10), ptr, 10);
        std::ptr::copy_nonoverlapping(ptr.add(10), ptr, 11);
        std::ptr::copy_nonoverlapping(ptr.add(10), ptr, 9);
        std::ptr::copy_nonoverlapping(ptr.add(10), ptr.add(10), 10);
    }
}
