use core::{borrow::Borrow, cell::RefCell};
use alloc::boxed::Box;

use crate::{
    acl::AclMgr,
    fabric::FabricMgr,
    handler_chain_type,
    mdns::Mdns,
    secure_channel::pake::PaseMgr,
    utils::{epoch::Epoch, rand::Rand},
};

use super::{
    cluster_basic_information::{self, BasicInfoCluster, BasicInfoConfig},
    objects::{Cluster, EmptyHandler, Endpoint, EndptId},
    sdm::{
        admin_commissioning::{self, AdminCommCluster},
        dev_att::DevAttDataFetcher,
        ethernet_nw_diagnostics::{self, EthNwDiagCluster},
        failsafe::FailSafe,
        general_commissioning::{self, GenCommCluster},
        general_diagnostics::{self, GenDiagCluster},
        group_key_management,
        group_key_management::GrpKeyMgmtCluster,
        noc::{self, NocCluster},
        nw_commissioning::{self, NwCommCluster},
    },
    system_model::{
        access_control::{self, AccessControlCluster},
        descriptor::{self, DescriptorCluster},
    },
};

pub type RootEndpointHandler<'a> = handler_chain_type!(
    DescriptorCluster<'static>,
    BasicInfoCluster<'a>,
    GenCommCluster<'a>,
    NwCommCluster,
    AdminCommCluster<'a>,
    NocCluster<'a>,
    AccessControlCluster<'a>,
    GenDiagCluster,
    EthNwDiagCluster,
    GrpKeyMgmtCluster
);

pub const CLUSTERS: [Cluster<'static>; 10] = [
    descriptor::CLUSTER,
    cluster_basic_information::CLUSTER,
    general_commissioning::CLUSTER,
    nw_commissioning::CLUSTER,
    admin_commissioning::CLUSTER,
    noc::CLUSTER,
    access_control::CLUSTER,
    general_diagnostics::CLUSTER,
    ethernet_nw_diagnostics::CLUSTER,
    group_key_management::CLUSTER,
];

pub const fn endpoint(id: EndptId) -> Endpoint<'static> {
    Endpoint {
        id,
        device_type: super::device_types::DEV_TYPE_ROOT_NODE,
        clusters: &CLUSTERS,
    }
}

pub fn handler<'a, T>(endpoint_id: u16, matter: &'a T) -> RootEndpointHandler<'a>
where
    T: Borrow<BasicInfoConfig<'a>>
        + Borrow<dyn DevAttDataFetcher + 'a>
        + Borrow<RefCell<PaseMgr>>
        + Borrow<RefCell<FabricMgr>>
        + Borrow<RefCell<AclMgr>>
        + Borrow<RefCell<FailSafe>>
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
        *matter.borrow(),
        *matter.borrow(),
        matter.borrow(),
    )
}

#[allow(clippy::too_many_arguments)]
pub fn wrap<'a>(
    endpoint_id: u16,
    basic_info: &'a BasicInfoConfig<'a>,
    dev_att: &'a dyn DevAttDataFetcher,
    pase: &'a RefCell<PaseMgr>,
    fabric: &'a RefCell<FabricMgr>,
    acl: &'a RefCell<AclMgr>,
    failsafe: &'a RefCell<FailSafe>,
    mdns: &'a dyn Mdns,
    epoch: Epoch,
    rand: Rand,
    clear_display_callback: &'a Option<Box<dyn Fn()>>,
) -> RootEndpointHandler<'a> {
    EmptyHandler
        .chain(
            endpoint_id,
            group_key_management::ID,
            GrpKeyMgmtCluster::new(rand),
        )
        .chain(
            endpoint_id,
            ethernet_nw_diagnostics::ID,
            EthNwDiagCluster::new(rand),
        )
        .chain(
            endpoint_id,
            general_diagnostics::ID,
            GenDiagCluster::new(rand),
        )
        .chain(
            endpoint_id,
            access_control::ID,
            AccessControlCluster::new(acl, rand),
        )
        .chain(
            endpoint_id,
            noc::ID,
            NocCluster::new(dev_att, fabric, acl, failsafe, mdns, epoch, rand),
        )
        .chain(
            endpoint_id,
            admin_commissioning::ID,
            AdminCommCluster::new(pase, mdns, rand),
        )
        .chain(endpoint_id, nw_commissioning::ID, NwCommCluster::new(rand))
        .chain(
            endpoint_id,
            general_commissioning::ID,
            GenCommCluster::new(failsafe, rand, clear_display_callback),
        )
        .chain(
            endpoint_id,
            cluster_basic_information::ID,
            BasicInfoCluster::new(basic_info, rand),
        )
        .chain(endpoint_id, descriptor::ID, DescriptorCluster::new(rand))
}
