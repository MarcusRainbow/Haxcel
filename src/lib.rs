pub mod exports;
mod process;

extern crate xladd;
extern crate winapi;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
