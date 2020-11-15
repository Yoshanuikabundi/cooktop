//! Cooktop converts MD topologies between formats

#![warn(
    missing_copy_implementations,
    // missing_docs,
    missing_debug_implementations,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unused_extern_crates,
)]

pub mod gromacs;
mod parser;

#[cfg(test)]
mod tests {}
