#![allow(
    clippy::deref_addrof,
    dead_code,
    unused,
    clippy::no_effect,
    clippy::unnecessary_struct_initialization
)]
#![warn(clippy::unnecessary_operation)]

struct Tuple(i32);
struct Struct {
    field: i32,
}
enum Enum {
    Tuple(i32),
    Struct { field: i32 },
}
struct DropStruct {
    field: i32,
}
impl Drop for DropStruct {
    fn drop(&mut self) {}
}
struct DropTuple(i32);
impl Drop for DropTuple {
    fn drop(&mut self) {}
}
enum DropEnum {
    Tuple(i32),
    Struct { field: i32 },
}
impl Drop for DropEnum {
    fn drop(&mut self) {}
}
struct FooString {
    s: String,
}

fn get_number() -> i32 {
    0
}

fn get_usize() -> usize {
    0
}
fn get_struct() -> Struct {
    Struct { field: 0 }
}
fn get_drop_struct() -> DropStruct {
    DropStruct { field: 0 }
}

fn main() {
    Tuple(get_number());
    Struct { field: get_number() };
    Struct { ..get_struct() };
    Enum::Tuple(get_number());
    Enum::Struct { field: get_number() };
    5 + get_number();
    *&get_number();
    &get_number();
    (5, 6, get_number());
    get_number()..;
    ..get_number();
    5..get_number();
    [42, get_number()];
    [42, 55][get_usize()];
    (42, get_number()).1;
    [get_number(); 55];
    [42; 55][get_usize()];
    {
        get_number()
    };
    FooString {
        s: String::from("blah"),
    };

    // Do not warn
    DropTuple(get_number());
    DropStruct { field: get_number() };
    DropStruct { field: get_number() };
    DropStruct { ..get_drop_struct() };
    DropEnum::Tuple(get_number());
    DropEnum::Struct { field: get_number() };

    // Issue #9954
    fn one() -> i8 {
        1
    }
    macro_rules! use_expr {
        ($($e:expr),*) => {{ $($e;)* }}
    }
    use_expr!(isize::MIN / -(one() as isize), i8::MIN / -one());
}

fn issue9951() {
    {
        // Original repro
        #[derive(Clone, Default)]
        struct MaybeCopy<T>(T);

        impl Copy for MaybeCopy<u8> {}

        // Since `MaybeCopy` is generic and there is only one `Copy` impl for it that doesn't use type
        // parameters, and array repeat expression requires the type implement `Copy`,
        // type inference can deduce that `MaybeCopy<?>` must be `MaybeCopy<u8>`.
        // However if we were to reduce it to just `MaybeCopy::default()`, there wouldn't be a `Copy`
        // constraint anymore, creating compile errors. Don't lint here!
        [MaybeCopy::default(); 13];

        // `Copy` is not required in array repeat expressions when array length is 0 or 1.
        // In that case, reducing it is ok because there's no such `Copy` constraint that would help
        // type inference here.
        [MaybeCopy::<u8>::default(); 0];
        //~^ ERROR: unnecessary operation
        [MaybeCopy::<u8>::default(); 1];
        //~^ ERROR: unnecessary operation
    }

    {
        #[derive(Default, Clone)]
        struct S<T>(T);

        impl<T: Copy> Copy for S<T> {}

        [S::<u8>::default(); 13];
        //~^ ERROR: unnecessary operation
        [S::<u8>::default(); 0];
        //~^ ERROR: unnecessary operation
    }

    {
        #[derive(Default, Clone)]
        struct S<T>(T);

        impl Copy for S<u8> {}
        impl Copy for S<u16> {}

        // Similar to a generic `S<T>: Copy` impl, there are multiple candidates, so we can lint.
        [S::<u8>::default(); 13];
        //~^ ERROR: unnecessary operation
        [S::<u8>::default(); 0];
        //~^ ERROR: unnecessary operation
    }

    {
        #[derive(Default, Clone)]
        struct S<T>(T);
        #[derive(Default, Clone)]
        struct S2<T>(T);

        impl<T: Copy> Copy for S<T> {}
        // Nested type parameter
        impl<T: Copy> Copy for S2<S<T>> {}

        [S2::<S<u8>>::default(); 13];
        //~^ ERROR: unnecessary operation
        [S2::<S<u8>>::default(); 0];
        //~^ ERROR: unnecessary operation
    }
}
