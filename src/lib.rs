use hs_bindgen::*;

use sphinx_recursion_gnark_ffi::ffi;

// Facade exposing a bindgen anchor
#[hs_bindgen]
pub fn verify_plonk_bn254(
    build_dir_str: &str,
    proof_str: &str,
    vkey_hash_str: &str,
    committed_values_digest_str: &str,
) -> u32 {
    // TODO: sanity-check the inputs by parsing the build_dir_str, vkey_hash_str, and committed_values_digest_str
    let res = ffi::verify_plonk_bn254(
        build_dir_str,
        proof_str,
        vkey_hash_str,
        committed_values_digest_str,
    );

    // Call the actual function in sphinx_recursion_gnark_ffi
    if let Err(e) = res {
        eprintln!("Error in verify_plonk_bn254: {e}");
        0u32 // Return 0 for failure
    } else {
        1u32 // Return 1 for success
    }
}

#[cfg(test)]
mod tests {
    use ffi::verify_plonk_bn254;
    use num_bigint::BigUint;
    use serde::{Deserialize, Serialize};
    use sha2::{Digest as _, Sha256};
    use std::{
        fs,
        path::{Path, PathBuf},
    };

    use super::*;

    // this assumes that the artifacts are installed in ~/.sp1/circuits/plonk_bn254/dev
    pub fn plonk_bn254_artifacts_dev_dir() -> PathBuf {
        home::home_dir()
            .unwrap()
            .join(".sp1")
            .join("circuits")
            .join("plonk_bn254")
            .join("v1.0.8-testnet")
    }

    pub fn asset_directory() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("assets")
    }

    fn is_fixture_file(entry: &Path) -> bool {
        entry.extension().and_then(|ext| ext.to_str()) == Some("json")
    }

    pub fn get_vkey_hash(build_dir: &Path) -> [u8; 32] {
        let vkey_path = build_dir.join("vk.bin");
        let vk_bin_bytes = std::fs::read(vkey_path).unwrap();
        Sha256::digest(vk_bin_bytes).into()
    }

    const MIN_PROOF_LENGTH: usize = 3*64 + 192 + 160 + 64 + 32 + 64 + 64; // 768

    #[test]
    fn test_plonk_bn254_prover() {
        let mut results: Vec<_> = Vec::new();

        // Iterate over all "*.fixture" files in the asset directory
        for entry in fs::read_dir(asset_directory()).expect("Failed to read assets directory") {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();

            // Check if the file has the ".json" extension
            if is_fixture_file(&path) {
                println!("Testing fixture: {:?}", path);

                // Load the fixture from the file
                let fixture_data =
                    std::fs::read_to_string(&path).expect("Failed to read fixture file");

                // Deserialize the Solidity fixture to use in the test
                let fixture: SolidityFixture =
                    serde_json::from_str(&fixture_data).expect("Failed to deserialize fixture");

                // Helper function to remove "0x" if present
                fn strip_hex_prefix(hex: &str) -> &str {
                    hex.strip_prefix("0x").unwrap_or(hex)
                }

                // Parse vkey, public values, and proof after stripping "0x"
                let vkey = strip_hex_prefix(&fixture.vkey);
                let public_values = strip_hex_prefix(&fixture.public_values);
                let proof = strip_hex_prefix(&fixture.proof);

                // Sanity-check the vkey
                let vkey_hash_part = &proof[..8]; // 8 hex chars = 4 bytes
                let encoded_proof_part = &proof[8..]; // The rest is encoded_proof

                // Convert both parts from hex to bytes
                let plonk_vkey_hash: Vec<u8> =
                    hex::decode(vkey_hash_part).expect("Failed to decode vkey hash");

                let vkey_hash = get_vkey_hash(&plonk_bn254_artifacts_dev_dir());
                assert_eq!(vkey_hash[..4], plonk_vkey_hash, "vkey hash mismatch! this proof was generated with a different version of the Plonk circuit. Proof requires {}, but Plonk circuit is at {}", vkey_hash_part, hex::encode(vkey_hash));

                // Sanity-check the proof length
                // l_com: 64 bytes,
                // r_com: 64 bytes,
                // o_com: 64 bytes,
                // h0, h1, h2: 64 bytes x 3 = 192 bytes,
                // l_at_zeta, r_at_zeta, o_at_zeta, s1_at_zeta, s2_at_zeta: 32 bytes x 5 = 160 bytes,
                // grand_product_commitment_x, grand_product_commitment_y: 32 bytes x 2 = 64 bytes,
                // grand_product_at_zeta_omega: 32 bytes,
                // skip the claimed value of the linearised polynomial at zeta, recomputed in the verifier
                // opening_at_zeta_proof: 64 bytes,
                // opening_at_zeta_omega_proof: 64 bytes,
                // selector_commit_api_at_zeta: variable,
                // wire_committed_commitments: variable,
                assert!(proof.len() >= MIN_PROOF_LENGTH);

                // Parse the BigNum arguments
                let vkey_biguint = BigUint::parse_bytes(vkey.as_bytes(), 16).expect("Failed to parse vkey");
                let public_values_biguint = BigUint::parse_bytes(public_values.as_bytes(), 16).expect("Failed to parse public values");
                        
                // Fetch the prover's asset directory
                let build_dir = plonk_bn254_artifacts_dev_dir();
                let result = verify_plonk_bn254(
                    build_dir.to_str().unwrap(),
                    &encoded_proof_part,
                    &vkey_biguint.to_string(),
                    &public_values_biguint.to_string(),
                );

                // Push the result of this verification to the results list
                match result {
                    Ok(_) => results.push(Ok(())),
                    Err(e) => {
                        results.push(Err(format!("Verification failed for {:?}: {:?}", path, e)))
                    }
                }

                println!("Test completed for fixture: {:?}", path);
            }
        }
        // Unwrap results after the loop
        for result in results.iter() {
            result.as_ref().unwrap();
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "camelCase")]
    struct SolidityFixture {
        vkey: String,
        public_values: String,
        proof: String,
    }
}
