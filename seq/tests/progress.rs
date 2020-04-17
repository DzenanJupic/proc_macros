#![feature(proc_macro_hygiene)]

#[test]
fn tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/01-parse-header.rs");
    t.pass("tests/02-parse-body.rs");
    t.compile_fail("tests/03-expand-four-errors.rs");
    t.pass("tests/04-paste-ident.rs");
    t.pass("tests/05-repeat-section.rs");
    t.pass("tests/06-make-work-in-function.rs");
    t.pass("tests/07-init-array.rs");
    t.pass("tests/08-inclusive-range.rs");
    t.compile_fail("tests/09-ident-span.rs");
    t.pass("tests/10-interaction-with-macrorules.rs");
}

use seq::eseq;

fn main() {
    let tuple = (9u8, 90u16, 900u32, 9000u64);

    let mut sum = 0;

    eseq!(N in 0..4 {{
        #(
            sum += tuple.N as u64;
        )*
    }});

    assert_eq!(sum, 9999);
}
