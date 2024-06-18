use super::cluster_basic_information::{self, BasicInfoCluster, BasicInfoConfig};
use super::objects::{Cluster, EmptyHandler, Endpoint, EndptId, HandlerCompat};
use super::sdm::admin_commissioning::{self, AdminCommCluster};
use super::sdm::dev_att::DevAttDataFetcher;
use super::sdm::ethernet_nw_diagnostics::{self, EthNwDiagCluster};
use super::sdm::failsafe::FailSafe;
use super::sdm::general_commissioning::{self, GenCommCluster};
use super::sdm::general_diagnostics::{self, GenDiagCluster};
use super::sdm::group_key_management::{self, GrpKeyMgmtCluster};
use super::sdm::noc::{self, NocCluster};
use super::sdm::nw_commissioning::{self, EthNwCommCluster};
use super::sdm::wifi_nw_diagnostics;
use super::system_model::access_control::{self, AccessControlCluster};
use super::system_model::descriptor::{self, DescriptorCluster};
use crate::acl::AclMgr;
use crate::fabric::FabricMgr;
use crate::handler_chain_type;
use crate::mdns::Mdns;
use crate::secure_channel::pake::PaseMgr;
use crate::transport::core::TransportMgr;
use crate::utils::epoch::Epoch;
use crate::utils::rand::Rand;
use alloc::boxed::Box;
use core::borrow::Borrow;
use core::cell::RefCell;

const ETH_NW_CLUSTERS: [Cluster<'static>; 10] = [
    descriptor::CLUSTER,
    cluster_basic_information::CLUSTER,
    general_commissioning::CLUSTER,
    nw_commissioning::ETH_CLUSTER,
    admin_commissioning::CLUSTER,
    noc::CLUSTER,
    access_control::CLUSTER,
    general_diagnostics::CLUSTER,
    ethernet_nw_diagnostics::CLUSTER,
    group_key_management::CLUSTER,
];

const WIFI_NW_CLUSTERS: [Cluster<'static>; 10] = [
    descriptor::CLUSTER,
    cluster_basic_information::CLUSTER,
    general_commissioning::CLUSTER,
    nw_commissioning::WIFI_CLUSTER,
    admin_commissioning::CLUSTER,
    noc::CLUSTER,
    access_control::CLUSTER,
    general_diagnostics::CLUSTER,
    wifi_nw_diagnostics::CLUSTER,
    group_key_management::CLUSTER,
];

/// The type of operational network (Ethernet, Wifi or (future) Thread)
/// for which root endpoint meta-data is being requested
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum OperNwType {
    Ethernet,
    Wifi,
}

/// A utility function to create a root (Endpoint 0) object using the requested operational network type.
pub const fn endpoint(id: EndptId, op_nw_type: OperNwType) -> Endpoint<'static> {
    Endpoint {
        id,
        device_type: super::device_types::DEV_TYPE_ROOT_NODE,
        clusters: clusters(op_nw_type),
    }
}

/// A utility function to return the clusters for a root (Endpoint 0) object using the requested operational network type.
pub const fn clusters(op_nw_type: OperNwType) -> &'static [Cluster<'static>] {
    match op_nw_type {
        OperNwType::Ethernet => &ETH_NW_CLUSTERS,
        OperNwType::Wifi => &WIFI_NW_CLUSTERS,
    }
}

/// A type alias for a root (Endpoint 0) handler using Ethernet as an operational network
pub type EthRootEndpointHandler<'a> = RootEndpointHandler<'a, EthNwCommCluster, EthNwDiagCluster>;

/// A type representing the type of the root (Endpoint 0) handler
/// which is generic over the operational transport clusters (i.e. Ethernet, Wifi or Thread)
pub type RootEndpointHandler<'a, NWCOMM, NWDIAG> = handler_chain_type!(
    NWCOMM,
    NWDIAG,
    HandlerCompat<descriptor::DescriptorCluster<'a>>,
    HandlerCompat<cluster_basic_information::BasicInfoCluster<'a>>,
    HandlerCompat<general_commissioning::GenCommCluster<'a>>,
    HandlerCompat<admin_commissioning::AdminCommCluster<'a>>,
    HandlerCompat<noc::NocCluster<'a>>,
    HandlerCompat<access_control::AccessControlCluster<'a>>,
    HandlerCompat<general_diagnostics::GenDiagCluster>,
    HandlerCompat<group_key_management::GrpKeyMgmtCluster>
);

/// A utility function to instantiate the root (Endpoint 0) handler using Ethernet as the operational network.
pub fn eth_handler<'a, T>(endpoint_id: u16, matter: &'a T) -> EthRootEndpointHandler<'a>
where
    T: Borrow<BasicInfoConfig<'a>>
        + Borrow<dyn DevAttDataFetcher + 'a>
        + Borrow<RefCell<PaseMgr>>
        + Borrow<RefCell<FabricMgr>>
        + Borrow<RefCell<AclMgr>>
        + Borrow<RefCell<FailSafe>>
        + Borrow<TransportMgr<'a>>
        + Borrow<dyn Mdns + 'a>
        + Borrow<Epoch>
        + Borrow<Rand>
        + Borrow<Option<Box<dyn Fn()>>>
        + 'a,
{
    handler(
        endpoint_id,
        matter,
        EthNwCommCluster::new(*matter.borrow()),
        ethernet_nw_diagnostics::ID,
        EthNwDiagCluster::new(*matter.borrow()),
        true,
    )
}

/// A utility function to instantiate the root (Endpoint 0) handler.
/// Besides a reference to the main `Matter` object, this function
/// needs user-supplied implementations of the network commissioning
/// and network diagnostics clusters.
//
// TODO: The borrow abstraction below is not of much use and only increases
// the size of the handlers, as they hold on to various managers instead
// of simply keeping a reference to the `Matter` object. Remove it in future.
pub fn handler<'a, NWCOMM, NWDIAG, T>(
    endpoint_id: u16,
    matter: &'a T,
    nwcomm: NWCOMM,
    nwdiag_id: u32,
    nwdiag: NWDIAG,
    supports_concurrent_connection: bool,
) -> RootEndpointHandler<'a, NWCOMM, NWDIAG>
where
    T: Borrow<BasicInfoConfig<'a>>
        + Borrow<dyn DevAttDataFetcher + 'a>
        + Borrow<RefCell<PaseMgr>>
        + Borrow<RefCell<FabricMgr>>
        + Borrow<RefCell<AclMgr>>
        + Borrow<RefCell<FailSafe>>
        + Borrow<TransportMgr<'a>>
        + Borrow<dyn Mdns + 'a>
        + Borrow<Epoch>
        + Borrow<Rand>
        + Borrow<Option<Box<dyn Fn()>>>
        + 'a,
{
    wrap(
        endpoint_id,
        matter.borrow(),
        matter.borrow(),
        matter.borrow(),
        matter.borrow(),
        matter.borrow(),
        matter.borrow(),
        matter.borrow(),
        matter.borrow(),
        *matter.borrow(),
        *matter.borrow(),
        nwcomm,
        nwdiag_id,
        nwdiag,
        supports_concurrent_connection,
        matter.borrow(),
    )
}

#[allow(clippy::too_many_arguments)]
fn wrap<'a, NWCOMM, NWDIAG>(
    endpoint_id: u16,
    basic_info: &'a BasicInfoConfig<'a>,
    dev_att: &'a dyn DevAttDataFetcher,
    pase: &'a RefCell<PaseMgr>,
    fabric: &'a RefCell<FabricMgr>,
    acl: &'a RefCell<AclMgr>,
    failsafe: &'a RefCell<FailSafe>,
    transport_mgr: &'a TransportMgr<'a>,
    mdns: &'a dyn Mdns,
    epoch: Epoch,
    rand: Rand,
    nwcomm: NWCOMM,
    nwdiag_id: u32,
    nwdiag: NWDIAG,
    supports_concurrent_connection: bool,
    clear_display_callback: &'a Option<Box<dyn Fn()>>,
) -> RootEndpointHandler<'a, NWCOMM, NWDIAG> {
    EmptyHandler
        .chain(
            endpoint_id,
            group_key_management::ID,
            HandlerCompat(GrpKeyMgmtCluster::new(rand)),
        )
        .chain(
            endpoint_id,
            general_diagnostics::ID,
            HandlerCompat(GenDiagCluster::new(rand)),
        )
        .chain(
            endpoint_id,
            access_control::ID,
            HandlerCompat(AccessControlCluster::new(acl, rand)),
        )
        .chain(
            endpoint_id,
            noc::ID,
            HandlerCompat(NocCluster::new(
                dev_att,
                fabric,
                acl,
                failsafe,
                transport_mgr,
                mdns,
                epoch,
                rand,
            )),
        )
        .chain(
            endpoint_id,
            admin_commissioning::ID,
            HandlerCompat(AdminCommCluster::new(pase, mdns, rand)),
        )
        .chain(
            endpoint_id,
            general_commissioning::ID,
            HandlerCompat(GenCommCluster::new(
                failsafe,
                supports_concurrent_connection,
                rand,
                clear_display_callback,
            )),
        )
        .chain(
            endpoint_id,
            cluster_basic_information::ID,
            HandlerCompat(BasicInfoCluster::new(basic_info, rand)),
        )
        .chain(
            endpoint_id,
            descriptor::ID,
            HandlerCompat(DescriptorCluster::new(rand)),
        )
        .chain(endpoint_id, nwdiag_id, nwdiag)
        .chain(endpoint_id, nw_commissioning::ID, nwcomm)
}
