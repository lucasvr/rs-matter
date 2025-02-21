/*
 *
 *    Copyright (c) 2020-2022 Project CHIP Authors
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

//! This module contains the logic for generating the pairing code and the QR code for easy pairing.

use bitflags::bitflags;

use log::info;
use alloc::boxed::Box;
use qr::no_optional_data;

use verhoeff::Verhoeff;

use crate::data_model::cluster_basic_information::BasicInfoConfig;
use crate::error::Error;
use crate::BasicCommData;

use self::code::{compute_pairing_code, pretty_print_pairing_code};
use self::qr::{compute_qr_code_text, print_qr_code};

pub mod code;
pub mod qr;
pub mod vendor_identifiers;

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct DiscoveryCapabilities: u8 {
        const SOFT_AP = 0x01;
        const BLE = 0x02;
        const IP = 0x04;
    }
}

impl Default for DiscoveryCapabilities {
    fn default() -> Self {
        Self::IP
    }
}

/// Prepares and prints the pairing code and the QR code for easy pairing.
pub fn print_pairing_code_and_qr(
    dev_det: &BasicInfoConfig,
    comm_data: &BasicCommData,
    discovery_capabilities: DiscoveryCapabilities,
    buf: &mut [u8],
    display_qrcode_callback: &Option<Box<dyn Fn(&str)>>,
) -> Result<(), Error> {
    let pairing_code = compute_pairing_code(comm_data);

    pretty_print_pairing_code(&pairing_code);

    let (qr_code, remaining_buf) = compute_qr_code_text(
        dev_det,
        comm_data,
        discovery_capabilities,
        no_optional_data,
        buf,
    )?;

    print_qr_code(qr_code, remaining_buf)?;

    // Invoke the callback to display the QR code on a display, if available
    if let Some(callback) = display_qrcode_callback {
        callback(qr_code);
    }

    Ok(())
}
