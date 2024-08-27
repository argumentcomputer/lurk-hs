use hs_bindgen::*;

use p3_baby_bear::BabyBear;
use p3_field::PrimeField32;
use p3_field::{extension::BinomialExtensionField, AbstractExtensionField, AbstractField, Field};

use sphinx_recursion_gnark_ffi::ffi;

//#[hs_bindgen]
//#[no_mangle]
//pub fn verify_plonk_bn254(
//    data_dir: &str,
//    proof: &str,
//    vkey_hash: &str,
//    committed_values_digest: &str,
//) -> u32 {
//    if ffi::verify_plonk_bn254(data_dir, proof, vkey_hash, committed_values_digest).is_ok() {
//        1u32
//    } else {
//        0u32
//    }
//}

#[hs_bindgen]
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

#[hs_bindgen]
#[no_mangle]
pub(crate) extern "C" fn babybearinv(a: u32) -> u32 {
    let a = BabyBear::from_wrapped_u32(a);
    a.inverse().as_canonical_u32()
}

#[hs_bindgen]
fn hello(name: &str) {
    println!("Hello, {name}!");
}
