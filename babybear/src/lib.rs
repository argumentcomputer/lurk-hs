// From https://github.com/argumentcomputer/sphinx/blob/dev/recursion/gnark-ffi/src/babybear.rs

use p3_baby_bear::BabyBear;
use p3_field::PrimeField32;
use p3_field::{extension::BinomialExtensionField, AbstractExtensionField, AbstractField, Field};

#[no_mangle]
pub(crate) extern "C" fn babybearextinv(a: u32, b: u32, c: u32, d: u32, i: u32) -> u32 {
    let a = BabyBear::from_wrapped_u32(a);
    let b = BabyBear::from_wrapped_u32(b);
    let c = BabyBear::from_wrapped_u32(c);
    let d = BabyBear::from_wrapped_u32(d);
    let inv = BinomialExtensionField::<BabyBear, 4>::from_base_slice(&[a, b, c, d]).inverse();
    let inv: &[BabyBear] = inv.as_base_slice();
    inv[i as usize].as_canonical_u32()
}

#[no_mangle]
pub(crate) extern "C" fn babybearinv(a: u32) -> u32 {
    let a = BabyBear::from_wrapped_u32(a);
    a.inverse().as_canonical_u32()
}
