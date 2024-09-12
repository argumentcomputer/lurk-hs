use hs_bindgen::*;
use num_bigint::BigUint;
use sha2::{Digest as _, Sha256};

use sphinx_recursion_gnark_ffi::ffi;

// Helper function to remove "0x" if present
fn strip_hex_prefix(hex: &str) -> &str {
    hex.strip_prefix("0x").unwrap_or(hex)
}

// Facade exposing a bindgen anchor
#[hs_bindgen]
pub fn verify_plonk_bn254(
    build_dir_str: &str,
    proof_str: &str,
    vkey_hash_str: &str,
    committed_values_digest_str: &str,
) -> u32 {
    let proof_str = strip_hex_prefix(proof_str);
    let vkey_hash_str = strip_hex_prefix(vkey_hash_str);
    let committed_values_digest_str = strip_hex_prefix(committed_values_digest_str);

    // Decode the hex-encoded string for public values
    let decoded_bytes =
        hex::decode(committed_values_digest_str).expect("Invalid committed values field");

    // Check the bit length (bytes * 8 should be 256 bits), hash if necessary
    let bit_length = decoded_bytes.len() * 8;

    let public_inputs: String = if bit_length > 256 {
        // The user has provided the committed values rather than the digest!
        // Let's reproduce the digest using the committed values
        //
        // Hash the value using SHA-256
        let mut hash: [u8; 32] = Sha256::digest(decoded_bytes).into();
        // Truncate to 253 bits by clearing the top 3 bits of the first byte
        hash[0] &= 0x1F; // 0x1F is 00011111 in binary, which clears the top 3 bits

        // Re-encode the truncated hash in hex
        hex::encode(hash)
    } else {
        committed_values_digest_str.to_string()
    };

    // Parse the BigNum arguments
    let vkey_biguint =
        BigUint::parse_bytes(vkey_hash_str.as_bytes(), 16).expect("Failed to parse vkey");
    let public_values_biguint =
        BigUint::parse_bytes(public_inputs.as_bytes(), 16).expect("Failed to parse public values");

    let res = ffi::verify_plonk_bn254(
        build_dir_str,
        proof_str,
        &vkey_biguint.to_string(),
        &public_values_biguint.to_string(),
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
    use serde::{Deserialize, Serialize};
    use std::{
        fs,
        path::{Path, PathBuf},
    };

    use super::*;

    // this assumes that the artifacts are installed in ~/.sp1/circuits/plonk_bn254/dev
    pub fn plonk_bn254_artifacts_dev_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("verifier-assets")
            .join("v1.0.8-testnet")
    }

    pub fn asset_directory() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("assets")
    }

    fn is_fixture_file(entry: &Path) -> bool {
        entry.extension().and_then(|ext| ext.to_str()) == Some("json")
    }

    const MIN_PROOF_LENGTH: usize = 3 * 64 + 192 + 160 + 64 + 32 + 64 + 64; // 768

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
                assert!(strip_hex_prefix(&fixture.proof).len() >= MIN_PROOF_LENGTH);

                // Fetch the prover's asset directory
                let build_dir = plonk_bn254_artifacts_dev_dir();
                let result = super::verify_plonk_bn254(
                    build_dir.to_str().unwrap(),
                    &fixture.proof,
                    &fixture.vkey,
                    &fixture.public_values,
                );

                // Push the result of this verification to the results list
                match result {
                    1 => results.push(Ok(())),
                    _ => results.push(Err(format!("Verification failed for {:?}", path))),
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
