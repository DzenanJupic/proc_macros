// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::bitfield;
use seq_macro::seq;

pub trait Specifier {
    type UINT;
    const BITS: u8;
    const BYTES: u8;
}

seq!(N in 1..=8 {
    pub enum B#N {}
    impl Specifier for B#N {
        type UINT = u8;
        const BITS: u8 = N;
        const BYTES: u8 = 1;
    }
});
seq!(N in 9..=16 {
    pub enum B#N {}
    impl Specifier for B#N {
        type UINT = u16;
        const BITS: u8 = N;
        const BYTES: u8 = 2;
    }
});
seq!(N in 17..=32 {
    pub enum B#N {}
    impl Specifier for B#N {
        type UINT = u32;
        const BITS: u8 = N;
        const BYTES: u8 = 4;
    }
});
seq!(N in 33..=64 {
    pub enum B#N {}
    impl Specifier for B#N {
        type UINT = u64;
        const BITS: u8 = N;
        const BYTES: u8 = 8;
    }
});
