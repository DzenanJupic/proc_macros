#[test]
fn tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/01-specifier-types.rs");
    t.pass("tests/02-storage.rs");
    t.pass("tests/03-accessors.rs");
    t.compile_fail("tests/04-multiple-of-8bits.rs");
    //t.pass("tests/05-accessor-signatures.rs");
    //t.pass("tests/06-enums.rs");
    //t.pass("tests/07-optional-discriminant.rs");
    //t.compile_fail("tests/08-non-power-of-two.rs");
    //t.compile_fail("tests/09-variant-out-of-range.rs");
    //t.pass("tests/10-bits-attribute.rs");
    //t.compile_fail("tests/11-bits-attribute-wrong.rs");
    //t.pass("tests/12-accessors-edge.rs");
}

use bitfield::*;

#[bitfield]
pub struct MyFourBytes {
    a: B1,
    b: B3,
    c: B4,
    d: B24,
}

fn main() {
    let mut bitfield = MyFourBytes::new();
    assert_eq!(0, bitfield.get_a());
    assert_eq!(0, bitfield.get_b());
    assert_eq!(0, bitfield.get_c());
    assert_eq!(0, bitfield.get_d());

    bitfield.set_c(14);
    assert_eq!(0, bitfield.get_a());
    assert_eq!(0, bitfield.get_b());
    assert_eq!(14, bitfield.get_c());
    assert_eq!(0, bitfield.get_d());
}
