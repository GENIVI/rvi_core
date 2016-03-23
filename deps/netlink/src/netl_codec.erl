-module(netl_codec).
-include("netl_codec.hrl").
-export([dec_protocol/1, enc_protocol/1]).
-export([dec_rtnetlink_rtm_protocol/1, enc_rtnetlink_rtm_protocol/1]).
-export([dec_ctm_msgtype_netlink/1, enc_ctm_msgtype_netlink/1]).
-export([dec_ctm_msgtype_ctnetlink/1, enc_ctm_msgtype_ctnetlink/1]).
-export([dec_family/1, enc_family/1]).
-export([dec_nlmsg_type/1, enc_nlmsg_type/1]).
-export([dec_ctnetlink_protoinfo_tcp_state/1, enc_ctnetlink_protoinfo_tcp_state/1]).
-export([dec_rtnetlink_rtm_type/1, enc_rtnetlink_rtm_type/1]).
-export([dec_rtnetlink_rtm_scope/1, enc_rtnetlink_rtm_scope/1]).
-export([dec_rtnetlink_rtm_table/1, enc_rtnetlink_rtm_table/1]).
-export([dec_rtnetlink_link_operstate/1, enc_rtnetlink_link_operstate/1]).
-export([dec_rtnetlink_link_linkmode/1, enc_rtnetlink_link_linkmode/1]).
-export([dec_arphrd/1, enc_arphrd/1]).
-export([dec_iff_flags/1, enc_iff_flags/1]).
-export([dec_nlm_flags/1, enc_nlm_flags/1]).
-export([dec_nlm_get_flags/1, enc_nlm_get_flags/1]).
-export([dec_nlm_new_flags/1, enc_nlm_new_flags/1]).
-export([dec_ctnetlink_status/1, enc_ctnetlink_status/1]).
-export([dec_ctnetlink_exp_flags/1, enc_ctnetlink_exp_flags/1]).
-export([dec_rtnetlink_rtm_flags/1, enc_rtnetlink_rtm_flags/1]).
-export([dec_rtnetlink_link_protinfo_flags/1, enc_rtnetlink_link_protinfo_flags/1]).
-export([dec_ifa_flags/1, enc_ifa_flags/1]).
-export([dec_ctm_msgtype_ctnetlink_exp/1, enc_ctm_msgtype_ctnetlink_exp/1]).
-export([dec_ctnetlink_tuple_proto/1, enc_ctnetlink_tuple_proto/1]).
-export([dec_ctnetlink_protoinfo/1, enc_ctnetlink_protoinfo/1]).
-export([dec_ctnetlink_exp_tuple_proto/1, enc_ctnetlink_exp_tuple_proto/1]).
-export([dec_rtnetlink_link_linkinfo/1, enc_rtnetlink_link_linkinfo/1]).
-export([dec_rtnetlink_link_protinfo/1, enc_rtnetlink_link_protinfo/1]).
-export([dec_ctnetlink/1, enc_ctnetlink/1]).
-export([dec_rtnetlink_link/1, enc_rtnetlink_link/1]).
-export([dec_ctnetlink_nat_seq_adj/1, enc_ctnetlink_nat_seq_adj/1]).
-export([dec_rtnetlink_neigh/1, enc_rtnetlink_neigh/1]).
-export([dec_rtnetlink_prefix/1, enc_rtnetlink_prefix/1]).
-export([dec_ctnetlink_tuple/1, enc_ctnetlink_tuple/1]).
-export([dec_ctnetlink_exp_tuple/1, enc_ctnetlink_exp_tuple/1]).
-export([dec_rtnetlink_route/1, enc_rtnetlink_route/1]).
-export([dec_ctnetlink_counters/1, enc_ctnetlink_counters/1]).
-export([dec_rtnetlink_route_metrics/1, enc_rtnetlink_route_metrics/1]).
-export([dec_rtnetlink_addr/1, enc_rtnetlink_addr/1]).
-export([dec_ctnetlink_tuple_ip/1, enc_ctnetlink_tuple_ip/1]).
-export([dec_ctnetlink_protoinfo_tcp/1, enc_ctnetlink_protoinfo_tcp/1]).
-export([dec_ctnetlink_help/1, enc_ctnetlink_help/1]).
-export([dec_ctnetlink_timestamp/1, enc_ctnetlink_timestamp/1]).
-export([dec_ctnetlink_exp_tuple_ip/1, enc_ctnetlink_exp_tuple_ip/1]).
-export([dec_ctnetlink_exp/1, enc_ctnetlink_exp/1]).
-export([dec_overrun/1, enc_overrun/1]).
-export([dec_newlink/1, enc_newlink/1]).
-export([dec_dellink/1, enc_dellink/1]).
-export([dec_getlink/1, enc_getlink/1]).
-export([dec_newneigh/1, enc_newneigh/1]).
-export([dec_delneigh/1, enc_delneigh/1]).
-export([dec_getneigh/1, enc_getneigh/1]).
-export([dec_ifaddrmsg/1, enc_ifaddrmsg/1]).
-export([dec_ifinfomsg/1, enc_ifinfomsg/1]).
-export([dec_rtmsg/1, enc_rtmsg/1]).
-export([dec_ndmsg/1, enc_ndmsg/1]).
-export([dec_newroute/1, enc_newroute/1]).
-export([dec_delroute/1, enc_delroute/1]).
-export([dec_getroute/1, enc_getroute/1]).
-export([dec_done/1, enc_done/1]).
-export([dec_nlmsghdr/1, enc_nlmsghdr/1]).
-export([dec_newaddr/1, enc_newaddr/1]).
-export([dec_deladdr/1, enc_deladdr/1]).
-export([dec_getaddr/1, enc_getaddr/1]).
-export([dec_error/1, enc_error/1]).
-export([dec_if_map/1, enc_if_map/1]).
-export([dec_noop/1, enc_noop/1]).
dec_protocol(0) -> ip;
dec_protocol(1) -> icmp;
dec_protocol(2) -> igmp;
dec_protocol(4) -> ipip;
dec_protocol(6) -> tcp;
dec_protocol(8) -> egp;
dec_protocol(12) -> pup;
dec_protocol(17) -> udp;
dec_protocol(22) -> idp;
dec_protocol(29) -> tp;
dec_protocol(33) -> dccp;
dec_protocol(41) -> ipv6;
dec_protocol(43) -> routing;
dec_protocol(44) -> fragment;
dec_protocol(46) -> rsvp;
dec_protocol(47) -> gre;
dec_protocol(50) -> esp;
dec_protocol(51) -> ah;
dec_protocol(58) -> icmpv6;
dec_protocol(59) -> none;
dec_protocol(60) -> dstopts;
dec_protocol(92) -> mtp;
dec_protocol(98) -> encap;
dec_protocol(103) -> pim;
dec_protocol(108) -> comp;
dec_protocol(132) -> sctp;
dec_protocol(136) -> udplite;
dec_protocol(255) -> raw;
dec_protocol(V) -> V.
enc_protocol(ip) -> 0;
enc_protocol(icmp) -> 1;
enc_protocol(igmp) -> 2;
enc_protocol(ipip) -> 4;
enc_protocol(tcp) -> 6;
enc_protocol(egp) -> 8;
enc_protocol(pup) -> 12;
enc_protocol(udp) -> 17;
enc_protocol(idp) -> 22;
enc_protocol(tp) -> 29;
enc_protocol(dccp) -> 33;
enc_protocol(ipv6) -> 41;
enc_protocol(routing) -> 43;
enc_protocol(fragment) -> 44;
enc_protocol(rsvp) -> 46;
enc_protocol(gre) -> 47;
enc_protocol(esp) -> 50;
enc_protocol(ah) -> 51;
enc_protocol(icmpv6) -> 58;
enc_protocol(none) -> 59;
enc_protocol(dstopts) -> 60;
enc_protocol(mtp) -> 92;
enc_protocol(encap) -> 98;
enc_protocol(pim) -> 103;
enc_protocol(comp) -> 108;
enc_protocol(sctp) -> 132;
enc_protocol(udplite) -> 136;
enc_protocol(raw) -> 255;
enc_protocol(V) when is_integer(V) -> V;
enc_protocol(E) -> erlang:error({undefined,E}).
dec_rtnetlink_rtm_protocol(0) -> unspec;
dec_rtnetlink_rtm_protocol(1) -> redirect;
dec_rtnetlink_rtm_protocol(2) -> kernel;
dec_rtnetlink_rtm_protocol(3) -> boot;
dec_rtnetlink_rtm_protocol(4) -> static;
dec_rtnetlink_rtm_protocol(8) -> gated;
dec_rtnetlink_rtm_protocol(9) -> ra;
dec_rtnetlink_rtm_protocol(10) -> mrt;
dec_rtnetlink_rtm_protocol(11) -> zebra;
dec_rtnetlink_rtm_protocol(12) -> bird;
dec_rtnetlink_rtm_protocol(13) -> dnrouted;
dec_rtnetlink_rtm_protocol(14) -> xorp;
dec_rtnetlink_rtm_protocol(15) -> ntk;
dec_rtnetlink_rtm_protocol(16) -> dhcp;
dec_rtnetlink_rtm_protocol(V) -> V.
enc_rtnetlink_rtm_protocol(unspec) -> 0;
enc_rtnetlink_rtm_protocol(redirect) -> 1;
enc_rtnetlink_rtm_protocol(kernel) -> 2;
enc_rtnetlink_rtm_protocol(boot) -> 3;
enc_rtnetlink_rtm_protocol(static) -> 4;
enc_rtnetlink_rtm_protocol(gated) -> 8;
enc_rtnetlink_rtm_protocol(ra) -> 9;
enc_rtnetlink_rtm_protocol(mrt) -> 10;
enc_rtnetlink_rtm_protocol(zebra) -> 11;
enc_rtnetlink_rtm_protocol(bird) -> 12;
enc_rtnetlink_rtm_protocol(dnrouted) -> 13;
enc_rtnetlink_rtm_protocol(xorp) -> 14;
enc_rtnetlink_rtm_protocol(ntk) -> 15;
enc_rtnetlink_rtm_protocol(dhcp) -> 16;
enc_rtnetlink_rtm_protocol(V) when is_integer(V) -> V;
enc_rtnetlink_rtm_protocol(E) -> erlang:error({undefined,E}).
dec_ctm_msgtype_netlink(1) -> noop;
dec_ctm_msgtype_netlink(2) -> error;
dec_ctm_msgtype_netlink(3) -> done;
dec_ctm_msgtype_netlink(4) -> overrun;
dec_ctm_msgtype_netlink(V) -> V.
enc_ctm_msgtype_netlink(noop) -> 1;
enc_ctm_msgtype_netlink(error) -> 2;
enc_ctm_msgtype_netlink(done) -> 3;
enc_ctm_msgtype_netlink(overrun) -> 4;
enc_ctm_msgtype_netlink(V) when is_integer(V) -> V;
enc_ctm_msgtype_netlink(E) -> erlang:error({undefined,E}).
dec_ctm_msgtype_ctnetlink(0) -> new;
dec_ctm_msgtype_ctnetlink(1) -> get;
dec_ctm_msgtype_ctnetlink(2) -> delete;
dec_ctm_msgtype_ctnetlink(3) -> get_ctrzero;
dec_ctm_msgtype_ctnetlink(V) -> V.
enc_ctm_msgtype_ctnetlink(new) -> 0;
enc_ctm_msgtype_ctnetlink(get) -> 1;
enc_ctm_msgtype_ctnetlink(delete) -> 2;
enc_ctm_msgtype_ctnetlink(get_ctrzero) -> 3;
enc_ctm_msgtype_ctnetlink(V) when is_integer(V) -> V;
enc_ctm_msgtype_ctnetlink(E) -> erlang:error({undefined,E}).
dec_family(0) -> unspec;
dec_family(1) -> local;
dec_family(2) -> inet;
dec_family(3) -> ax25;
dec_family(4) -> ipx;
dec_family(5) -> appletalk;
dec_family(6) -> netrom;
dec_family(7) -> bridge;
dec_family(8) -> atmpvc;
dec_family(9) -> x25;
dec_family(10) -> inet6;
dec_family(11) -> rose;
dec_family(12) -> decnet;
dec_family(13) -> netbeui;
dec_family(14) -> security;
dec_family(15) -> key;
dec_family(16) -> netlink;
dec_family(17) -> packet;
dec_family(18) -> ash;
dec_family(19) -> econet;
dec_family(20) -> atmsvc;
dec_family(21) -> rds;
dec_family(22) -> sna;
dec_family(23) -> irda;
dec_family(24) -> pppox;
dec_family(25) -> wanpipe;
dec_family(26) -> llc;
dec_family(29) -> can;
dec_family(30) -> tipc;
dec_family(31) -> bluetooth;
dec_family(32) -> iucv;
dec_family(33) -> rxrpc;
dec_family(34) -> isdn;
dec_family(35) -> phonet;
dec_family(36) -> ieee802154;
dec_family(V) -> V.
enc_family(unspec) -> 0;
enc_family(local) -> 1;
enc_family(inet) -> 2;
enc_family(ax25) -> 3;
enc_family(ipx) -> 4;
enc_family(appletalk) -> 5;
enc_family(netrom) -> 6;
enc_family(bridge) -> 7;
enc_family(atmpvc) -> 8;
enc_family(x25) -> 9;
enc_family(inet6) -> 10;
enc_family(rose) -> 11;
enc_family(decnet) -> 12;
enc_family(netbeui) -> 13;
enc_family(security) -> 14;
enc_family(key) -> 15;
enc_family(netlink) -> 16;
enc_family(packet) -> 17;
enc_family(ash) -> 18;
enc_family(econet) -> 19;
enc_family(atmsvc) -> 20;
enc_family(rds) -> 21;
enc_family(sna) -> 22;
enc_family(irda) -> 23;
enc_family(pppox) -> 24;
enc_family(wanpipe) -> 25;
enc_family(llc) -> 26;
enc_family(can) -> 29;
enc_family(tipc) -> 30;
enc_family(bluetooth) -> 31;
enc_family(iucv) -> 32;
enc_family(rxrpc) -> 33;
enc_family(isdn) -> 34;
enc_family(phonet) -> 35;
enc_family(ieee802154) -> 36;
enc_family(V) when is_integer(V) -> V;
enc_family(E) -> erlang:error({undefined,E}).
dec_nlmsg_type(1) -> noop;
dec_nlmsg_type(2) -> error;
dec_nlmsg_type(3) -> done;
dec_nlmsg_type(4) -> overrun;
dec_nlmsg_type(16) -> newlink;
dec_nlmsg_type(17) -> dellink;
dec_nlmsg_type(18) -> getlink;
dec_nlmsg_type(19) -> setlink;
dec_nlmsg_type(20) -> newaddr;
dec_nlmsg_type(21) -> deladdr;
dec_nlmsg_type(22) -> getaddr;
dec_nlmsg_type(24) -> newroute;
dec_nlmsg_type(25) -> delroute;
dec_nlmsg_type(26) -> getroute;
dec_nlmsg_type(28) -> newneigh;
dec_nlmsg_type(29) -> delneigh;
dec_nlmsg_type(30) -> getneigh;
dec_nlmsg_type(32) -> newrule;
dec_nlmsg_type(33) -> delrule;
dec_nlmsg_type(34) -> getrule;
dec_nlmsg_type(36) -> newqdisc;
dec_nlmsg_type(37) -> delqdisc;
dec_nlmsg_type(38) -> getqdisc;
dec_nlmsg_type(40) -> newtclass;
dec_nlmsg_type(41) -> deltclass;
dec_nlmsg_type(42) -> gettclass;
dec_nlmsg_type(44) -> newtfilter;
dec_nlmsg_type(45) -> deltfilter;
dec_nlmsg_type(46) -> gettfilter;
dec_nlmsg_type(48) -> newaction;
dec_nlmsg_type(49) -> delaction;
dec_nlmsg_type(50) -> getaction;
dec_nlmsg_type(52) -> newprefix;
dec_nlmsg_type(58) -> getmulticast;
dec_nlmsg_type(62) -> getanycast;
dec_nlmsg_type(64) -> newneightbl;
dec_nlmsg_type(66) -> getneightbl;
dec_nlmsg_type(67) -> setneightbl;
dec_nlmsg_type(68) -> newnduseropt;
dec_nlmsg_type(72) -> newaddrlabel;
dec_nlmsg_type(73) -> deladdrlabel;
dec_nlmsg_type(74) -> getaddrlabel;
dec_nlmsg_type(78) -> getdcb;
dec_nlmsg_type(79) -> setdcb;
dec_nlmsg_type(V) -> V.
enc_nlmsg_type(noop) -> 1;
enc_nlmsg_type(error) -> 2;
enc_nlmsg_type(done) -> 3;
enc_nlmsg_type(overrun) -> 4;
enc_nlmsg_type(newlink) -> 16;
enc_nlmsg_type(dellink) -> 17;
enc_nlmsg_type(getlink) -> 18;
enc_nlmsg_type(setlink) -> 19;
enc_nlmsg_type(newaddr) -> 20;
enc_nlmsg_type(deladdr) -> 21;
enc_nlmsg_type(getaddr) -> 22;
enc_nlmsg_type(newroute) -> 24;
enc_nlmsg_type(delroute) -> 25;
enc_nlmsg_type(getroute) -> 26;
enc_nlmsg_type(newneigh) -> 28;
enc_nlmsg_type(delneigh) -> 29;
enc_nlmsg_type(getneigh) -> 30;
enc_nlmsg_type(newrule) -> 32;
enc_nlmsg_type(delrule) -> 33;
enc_nlmsg_type(getrule) -> 34;
enc_nlmsg_type(newqdisc) -> 36;
enc_nlmsg_type(delqdisc) -> 37;
enc_nlmsg_type(getqdisc) -> 38;
enc_nlmsg_type(newtclass) -> 40;
enc_nlmsg_type(deltclass) -> 41;
enc_nlmsg_type(gettclass) -> 42;
enc_nlmsg_type(newtfilter) -> 44;
enc_nlmsg_type(deltfilter) -> 45;
enc_nlmsg_type(gettfilter) -> 46;
enc_nlmsg_type(newaction) -> 48;
enc_nlmsg_type(delaction) -> 49;
enc_nlmsg_type(getaction) -> 50;
enc_nlmsg_type(newprefix) -> 52;
enc_nlmsg_type(getmulticast) -> 58;
enc_nlmsg_type(getanycast) -> 62;
enc_nlmsg_type(newneightbl) -> 64;
enc_nlmsg_type(getneightbl) -> 66;
enc_nlmsg_type(setneightbl) -> 67;
enc_nlmsg_type(newnduseropt) -> 68;
enc_nlmsg_type(newaddrlabel) -> 72;
enc_nlmsg_type(deladdrlabel) -> 73;
enc_nlmsg_type(getaddrlabel) -> 74;
enc_nlmsg_type(getdcb) -> 78;
enc_nlmsg_type(setdcb) -> 79;
enc_nlmsg_type(V) when is_integer(V) -> V;
enc_nlmsg_type(E) -> erlang:error({undefined,E}).
dec_ctnetlink_protoinfo_tcp_state(0) -> none;
dec_ctnetlink_protoinfo_tcp_state(1) -> syn_sent;
dec_ctnetlink_protoinfo_tcp_state(2) -> syn_recv;
dec_ctnetlink_protoinfo_tcp_state(3) -> established;
dec_ctnetlink_protoinfo_tcp_state(4) -> fin_wait;
dec_ctnetlink_protoinfo_tcp_state(5) -> close_wait;
dec_ctnetlink_protoinfo_tcp_state(6) -> last_ack;
dec_ctnetlink_protoinfo_tcp_state(7) -> time_wait;
dec_ctnetlink_protoinfo_tcp_state(8) -> close;
dec_ctnetlink_protoinfo_tcp_state(9) -> listen;
dec_ctnetlink_protoinfo_tcp_state(10) -> max;
dec_ctnetlink_protoinfo_tcp_state(11) -> ignore;
dec_ctnetlink_protoinfo_tcp_state(V) -> V.
enc_ctnetlink_protoinfo_tcp_state(none) -> 0;
enc_ctnetlink_protoinfo_tcp_state(syn_sent) -> 1;
enc_ctnetlink_protoinfo_tcp_state(syn_recv) -> 2;
enc_ctnetlink_protoinfo_tcp_state(established) -> 3;
enc_ctnetlink_protoinfo_tcp_state(fin_wait) -> 4;
enc_ctnetlink_protoinfo_tcp_state(close_wait) -> 5;
enc_ctnetlink_protoinfo_tcp_state(last_ack) -> 6;
enc_ctnetlink_protoinfo_tcp_state(time_wait) -> 7;
enc_ctnetlink_protoinfo_tcp_state(close) -> 8;
enc_ctnetlink_protoinfo_tcp_state(listen) -> 9;
enc_ctnetlink_protoinfo_tcp_state(max) -> 10;
enc_ctnetlink_protoinfo_tcp_state(ignore) -> 11;
enc_ctnetlink_protoinfo_tcp_state(V) when is_integer(V) -> V;
enc_ctnetlink_protoinfo_tcp_state(E) -> erlang:error({undefined,E}).
dec_rtnetlink_rtm_type(0) -> unspec;
dec_rtnetlink_rtm_type(1) -> unicast;
dec_rtnetlink_rtm_type(2) -> local;
dec_rtnetlink_rtm_type(3) -> broadcast;
dec_rtnetlink_rtm_type(4) -> anycast;
dec_rtnetlink_rtm_type(5) -> multicast;
dec_rtnetlink_rtm_type(6) -> blackhole;
dec_rtnetlink_rtm_type(7) -> unreachable;
dec_rtnetlink_rtm_type(8) -> prohibit;
dec_rtnetlink_rtm_type(9) -> throw;
dec_rtnetlink_rtm_type(10) -> nat;
dec_rtnetlink_rtm_type(11) -> xresolve;
dec_rtnetlink_rtm_type(V) -> V.
enc_rtnetlink_rtm_type(unspec) -> 0;
enc_rtnetlink_rtm_type(unicast) -> 1;
enc_rtnetlink_rtm_type(local) -> 2;
enc_rtnetlink_rtm_type(broadcast) -> 3;
enc_rtnetlink_rtm_type(anycast) -> 4;
enc_rtnetlink_rtm_type(multicast) -> 5;
enc_rtnetlink_rtm_type(blackhole) -> 6;
enc_rtnetlink_rtm_type(unreachable) -> 7;
enc_rtnetlink_rtm_type(prohibit) -> 8;
enc_rtnetlink_rtm_type(throw) -> 9;
enc_rtnetlink_rtm_type(nat) -> 10;
enc_rtnetlink_rtm_type(xresolve) -> 11;
enc_rtnetlink_rtm_type(V) when is_integer(V) -> V;
enc_rtnetlink_rtm_type(E) -> erlang:error({undefined,E}).
dec_rtnetlink_rtm_scope(0) -> universe;
dec_rtnetlink_rtm_scope(200) -> site;
dec_rtnetlink_rtm_scope(253) -> link;
dec_rtnetlink_rtm_scope(254) -> host;
dec_rtnetlink_rtm_scope(255) -> nowhere;
dec_rtnetlink_rtm_scope(V) -> V.
enc_rtnetlink_rtm_scope(universe) -> 0;
enc_rtnetlink_rtm_scope(site) -> 200;
enc_rtnetlink_rtm_scope(link) -> 253;
enc_rtnetlink_rtm_scope(host) -> 254;
enc_rtnetlink_rtm_scope(nowhere) -> 255;
enc_rtnetlink_rtm_scope(V) when is_integer(V) -> V;
enc_rtnetlink_rtm_scope(E) -> erlang:error({undefined,E}).
dec_rtnetlink_rtm_table(0) -> unspec;
dec_rtnetlink_rtm_table(252) -> compat;
dec_rtnetlink_rtm_table(253) -> default;
dec_rtnetlink_rtm_table(254) -> main;
dec_rtnetlink_rtm_table(255) -> local;
dec_rtnetlink_rtm_table(V) -> V.
enc_rtnetlink_rtm_table(unspec) -> 0;
enc_rtnetlink_rtm_table(compat) -> 252;
enc_rtnetlink_rtm_table(default) -> 253;
enc_rtnetlink_rtm_table(main) -> 254;
enc_rtnetlink_rtm_table(local) -> 255;
enc_rtnetlink_rtm_table(V) when is_integer(V) -> V;
enc_rtnetlink_rtm_table(E) -> erlang:error({undefined,E}).
dec_rtnetlink_link_operstate(0) -> unknown;
dec_rtnetlink_link_operstate(1) -> notpresent;
dec_rtnetlink_link_operstate(2) -> down;
dec_rtnetlink_link_operstate(3) -> lowerlayerdown;
dec_rtnetlink_link_operstate(4) -> testing;
dec_rtnetlink_link_operstate(5) -> dormant;
dec_rtnetlink_link_operstate(6) -> up;
dec_rtnetlink_link_operstate(V) -> V.
enc_rtnetlink_link_operstate(unknown) -> 0;
enc_rtnetlink_link_operstate(notpresent) -> 1;
enc_rtnetlink_link_operstate(down) -> 2;
enc_rtnetlink_link_operstate(lowerlayerdown) -> 3;
enc_rtnetlink_link_operstate(testing) -> 4;
enc_rtnetlink_link_operstate(dormant) -> 5;
enc_rtnetlink_link_operstate(up) -> 6;
enc_rtnetlink_link_operstate(V) when is_integer(V) -> V;
enc_rtnetlink_link_operstate(E) -> erlang:error({undefined,E}).
dec_rtnetlink_link_linkmode(0) -> default;
dec_rtnetlink_link_linkmode(1) -> dormant;
dec_rtnetlink_link_linkmode(V) -> V.
enc_rtnetlink_link_linkmode(default) -> 0;
enc_rtnetlink_link_linkmode(dormant) -> 1;
enc_rtnetlink_link_linkmode(V) when is_integer(V) -> V;
enc_rtnetlink_link_linkmode(E) -> erlang:error({undefined,E}).
dec_arphrd(0) -> netrom;
dec_arphrd(1) -> ether;
dec_arphrd(2) -> eether;
dec_arphrd(3) -> ax25;
dec_arphrd(4) -> pronet;
dec_arphrd(5) -> chaos;
dec_arphrd(6) -> ieee802;
dec_arphrd(7) -> arcnet;
dec_arphrd(8) -> appletlk;
dec_arphrd(15) -> dlci;
dec_arphrd(19) -> atm;
dec_arphrd(23) -> metricom;
dec_arphrd(24) -> ieee1394;
dec_arphrd(27) -> eui64;
dec_arphrd(32) -> infiniband;
dec_arphrd(256) -> slip;
dec_arphrd(257) -> cslip;
dec_arphrd(258) -> slip6;
dec_arphrd(259) -> cslip6;
dec_arphrd(260) -> rsrvd;
dec_arphrd(264) -> adapt;
dec_arphrd(270) -> rose;
dec_arphrd(271) -> x25;
dec_arphrd(272) -> hwx25;
dec_arphrd(280) -> can;
dec_arphrd(512) -> ppp;
dec_arphrd(513) -> hdlc;
dec_arphrd(516) -> lapb;
dec_arphrd(517) -> ddcmp;
dec_arphrd(518) -> rawhdlc;
dec_arphrd(768) -> tunnel;
dec_arphrd(769) -> tunnel6;
dec_arphrd(770) -> frad;
dec_arphrd(771) -> skip;
dec_arphrd(772) -> loopback;
dec_arphrd(773) -> localtlk;
dec_arphrd(774) -> fddi;
dec_arphrd(775) -> bif;
dec_arphrd(776) -> sit;
dec_arphrd(777) -> ipddp;
dec_arphrd(778) -> ipgre;
dec_arphrd(779) -> pimreg;
dec_arphrd(780) -> hippi;
dec_arphrd(781) -> ash;
dec_arphrd(782) -> econet;
dec_arphrd(783) -> irda;
dec_arphrd(784) -> fcpp;
dec_arphrd(785) -> fcal;
dec_arphrd(786) -> fcpl;
dec_arphrd(787) -> fcfabric;
dec_arphrd(800) -> ieee802_tr;
dec_arphrd(801) -> ieee80211;
dec_arphrd(802) -> ieee80211_prism;
dec_arphrd(803) -> ieee80211_radiotap;
dec_arphrd(804) -> ieee802154;
dec_arphrd(820) -> phonet;
dec_arphrd(821) -> phonet_pipe;
dec_arphrd(822) -> caif;
dec_arphrd(65535) -> void;
dec_arphrd(65534) -> none;
dec_arphrd(V) -> V.
enc_arphrd(netrom) -> 0;
enc_arphrd(ether) -> 1;
enc_arphrd(eether) -> 2;
enc_arphrd(ax25) -> 3;
enc_arphrd(pronet) -> 4;
enc_arphrd(chaos) -> 5;
enc_arphrd(ieee802) -> 6;
enc_arphrd(arcnet) -> 7;
enc_arphrd(appletlk) -> 8;
enc_arphrd(dlci) -> 15;
enc_arphrd(atm) -> 19;
enc_arphrd(metricom) -> 23;
enc_arphrd(ieee1394) -> 24;
enc_arphrd(eui64) -> 27;
enc_arphrd(infiniband) -> 32;
enc_arphrd(slip) -> 256;
enc_arphrd(cslip) -> 257;
enc_arphrd(slip6) -> 258;
enc_arphrd(cslip6) -> 259;
enc_arphrd(rsrvd) -> 260;
enc_arphrd(adapt) -> 264;
enc_arphrd(rose) -> 270;
enc_arphrd(x25) -> 271;
enc_arphrd(hwx25) -> 272;
enc_arphrd(can) -> 280;
enc_arphrd(ppp) -> 512;
enc_arphrd(hdlc) -> 513;
enc_arphrd(lapb) -> 516;
enc_arphrd(ddcmp) -> 517;
enc_arphrd(rawhdlc) -> 518;
enc_arphrd(tunnel) -> 768;
enc_arphrd(tunnel6) -> 769;
enc_arphrd(frad) -> 770;
enc_arphrd(skip) -> 771;
enc_arphrd(loopback) -> 772;
enc_arphrd(localtlk) -> 773;
enc_arphrd(fddi) -> 774;
enc_arphrd(bif) -> 775;
enc_arphrd(sit) -> 776;
enc_arphrd(ipddp) -> 777;
enc_arphrd(ipgre) -> 778;
enc_arphrd(pimreg) -> 779;
enc_arphrd(hippi) -> 780;
enc_arphrd(ash) -> 781;
enc_arphrd(econet) -> 782;
enc_arphrd(irda) -> 783;
enc_arphrd(fcpp) -> 784;
enc_arphrd(fcal) -> 785;
enc_arphrd(fcpl) -> 786;
enc_arphrd(fcfabric) -> 787;
enc_arphrd(ieee802_tr) -> 800;
enc_arphrd(ieee80211) -> 801;
enc_arphrd(ieee80211_prism) -> 802;
enc_arphrd(ieee80211_radiotap) -> 803;
enc_arphrd(ieee802154) -> 804;
enc_arphrd(phonet) -> 820;
enc_arphrd(phonet_pipe) -> 821;
enc_arphrd(caif) -> 822;
enc_arphrd(void) -> 65535;
enc_arphrd(none) -> 65534;
enc_arphrd(V) when is_integer(V) -> V;
enc_arphrd(E) -> erlang:error({undefined,E}).
dec_iff_flags(0) -> up;
dec_iff_flags(1) -> broadcast;
dec_iff_flags(2) -> debug;
dec_iff_flags(3) -> loopback;
dec_iff_flags(4) -> pointopoint;
dec_iff_flags(5) -> notrailers;
dec_iff_flags(6) -> running;
dec_iff_flags(7) -> noarp;
dec_iff_flags(8) -> promisc;
dec_iff_flags(9) -> allmulti;
dec_iff_flags(10) -> master;
dec_iff_flags(11) -> slave;
dec_iff_flags(12) -> multicast;
dec_iff_flags(13) -> portsel;
dec_iff_flags(14) -> automedia;
dec_iff_flags(15) -> ynamic;
dec_iff_flags(16) -> lower_up;
dec_iff_flags(17) -> dormant;
dec_iff_flags(18) -> echo;
dec_iff_flags(V) -> V.
enc_iff_flags(up) -> 0;
enc_iff_flags(broadcast) -> 1;
enc_iff_flags(debug) -> 2;
enc_iff_flags(loopback) -> 3;
enc_iff_flags(pointopoint) -> 4;
enc_iff_flags(notrailers) -> 5;
enc_iff_flags(running) -> 6;
enc_iff_flags(noarp) -> 7;
enc_iff_flags(promisc) -> 8;
enc_iff_flags(allmulti) -> 9;
enc_iff_flags(master) -> 10;
enc_iff_flags(slave) -> 11;
enc_iff_flags(multicast) -> 12;
enc_iff_flags(portsel) -> 13;
enc_iff_flags(automedia) -> 14;
enc_iff_flags(ynamic) -> 15;
enc_iff_flags(lower_up) -> 16;
enc_iff_flags(dormant) -> 17;
enc_iff_flags(echo) -> 18;
enc_iff_flags(V) when is_integer(V) -> V;
enc_iff_flags(E) -> erlang:error({undefined,E}).
dec_nlm_flags(0) -> request;
dec_nlm_flags(1) -> multi;
dec_nlm_flags(2) -> ack;
dec_nlm_flags(3) -> echo;
dec_nlm_flags(V) -> V.
enc_nlm_flags(request) -> 0;
enc_nlm_flags(multi) -> 1;
enc_nlm_flags(ack) -> 2;
enc_nlm_flags(echo) -> 3;
enc_nlm_flags(V) when is_integer(V) -> V;
enc_nlm_flags(E) -> erlang:error({undefined,E}).
dec_nlm_get_flags(0) -> request;
dec_nlm_get_flags(1) -> multi;
dec_nlm_get_flags(2) -> ack;
dec_nlm_get_flags(3) -> echo;
dec_nlm_get_flags(8) -> root;
dec_nlm_get_flags(9) -> match;
dec_nlm_get_flags(10) -> atomic;
dec_nlm_get_flags(V) -> V.
enc_nlm_get_flags(request) -> 0;
enc_nlm_get_flags(multi) -> 1;
enc_nlm_get_flags(ack) -> 2;
enc_nlm_get_flags(echo) -> 3;
enc_nlm_get_flags(root) -> 8;
enc_nlm_get_flags(match) -> 9;
enc_nlm_get_flags(atomic) -> 10;
enc_nlm_get_flags(V) when is_integer(V) -> V;
enc_nlm_get_flags(E) -> erlang:error({undefined,E}).
dec_nlm_new_flags(0) -> request;
dec_nlm_new_flags(1) -> multi;
dec_nlm_new_flags(2) -> ack;
dec_nlm_new_flags(3) -> echo;
dec_nlm_new_flags(8) -> replace;
dec_nlm_new_flags(9) -> excl;
dec_nlm_new_flags(10) -> create;
dec_nlm_new_flags(11) -> append;
dec_nlm_new_flags(V) -> V.
enc_nlm_new_flags(request) -> 0;
enc_nlm_new_flags(multi) -> 1;
enc_nlm_new_flags(ack) -> 2;
enc_nlm_new_flags(echo) -> 3;
enc_nlm_new_flags(replace) -> 8;
enc_nlm_new_flags(excl) -> 9;
enc_nlm_new_flags(create) -> 10;
enc_nlm_new_flags(append) -> 11;
enc_nlm_new_flags(V) when is_integer(V) -> V;
enc_nlm_new_flags(E) -> erlang:error({undefined,E}).
dec_ctnetlink_status(0) -> expected;
dec_ctnetlink_status(1) -> seen_reply;
dec_ctnetlink_status(2) -> assured;
dec_ctnetlink_status(3) -> confirmed;
dec_ctnetlink_status(4) -> src_nat;
dec_ctnetlink_status(5) -> dst_nat;
dec_ctnetlink_status(6) -> seq_adjust;
dec_ctnetlink_status(7) -> src_nat_done;
dec_ctnetlink_status(8) -> dst_nat_done;
dec_ctnetlink_status(9) -> dying;
dec_ctnetlink_status(10) -> fixed_timeout;
dec_ctnetlink_status(V) -> V.
enc_ctnetlink_status(expected) -> 0;
enc_ctnetlink_status(seen_reply) -> 1;
enc_ctnetlink_status(assured) -> 2;
enc_ctnetlink_status(confirmed) -> 3;
enc_ctnetlink_status(src_nat) -> 4;
enc_ctnetlink_status(dst_nat) -> 5;
enc_ctnetlink_status(seq_adjust) -> 6;
enc_ctnetlink_status(src_nat_done) -> 7;
enc_ctnetlink_status(dst_nat_done) -> 8;
enc_ctnetlink_status(dying) -> 9;
enc_ctnetlink_status(fixed_timeout) -> 10;
enc_ctnetlink_status(V) when is_integer(V) -> V;
enc_ctnetlink_status(E) -> erlang:error({undefined,E}).
dec_ctnetlink_exp_flags(0) -> permanent;
dec_ctnetlink_exp_flags(1) -> inactive;
dec_ctnetlink_exp_flags(2) -> userspace;
dec_ctnetlink_exp_flags(V) -> V.
enc_ctnetlink_exp_flags(permanent) -> 0;
enc_ctnetlink_exp_flags(inactive) -> 1;
enc_ctnetlink_exp_flags(userspace) -> 2;
enc_ctnetlink_exp_flags(V) when is_integer(V) -> V;
enc_ctnetlink_exp_flags(E) -> erlang:error({undefined,E}).
dec_rtnetlink_rtm_flags(256) -> notify;
dec_rtnetlink_rtm_flags(512) -> cloned;
dec_rtnetlink_rtm_flags(1024) -> equalize;
dec_rtnetlink_rtm_flags(2048) -> prefix;
dec_rtnetlink_rtm_flags(V) -> V.
enc_rtnetlink_rtm_flags(notify) -> 256;
enc_rtnetlink_rtm_flags(cloned) -> 512;
enc_rtnetlink_rtm_flags(equalize) -> 1024;
enc_rtnetlink_rtm_flags(prefix) -> 2048;
enc_rtnetlink_rtm_flags(V) when is_integer(V) -> V;
enc_rtnetlink_rtm_flags(E) -> erlang:error({undefined,E}).
dec_rtnetlink_link_protinfo_flags(4) -> rs_sent;
dec_rtnetlink_link_protinfo_flags(5) -> ra_rcvd;
dec_rtnetlink_link_protinfo_flags(6) -> ra_managed;
dec_rtnetlink_link_protinfo_flags(7) -> ra_othercon;
dec_rtnetlink_link_protinfo_flags(31) -> ready;
dec_rtnetlink_link_protinfo_flags(V) -> V.
enc_rtnetlink_link_protinfo_flags(rs_sent) -> 4;
enc_rtnetlink_link_protinfo_flags(ra_rcvd) -> 5;
enc_rtnetlink_link_protinfo_flags(ra_managed) -> 6;
enc_rtnetlink_link_protinfo_flags(ra_othercon) -> 7;
enc_rtnetlink_link_protinfo_flags(ready) -> 31;
enc_rtnetlink_link_protinfo_flags(V) when is_integer(V) -> V;
enc_rtnetlink_link_protinfo_flags(E) -> erlang:error({undefined,E}).
dec_ifa_flags(0) -> secondary;
dec_ifa_flags(1) -> nodad;
dec_ifa_flags(2) -> optimistic;
dec_ifa_flags(3) -> dadfailed;
dec_ifa_flags(4) -> homeaddress;
dec_ifa_flags(5) -> deprecated;
dec_ifa_flags(6) -> tentative;
dec_ifa_flags(7) -> permanent;
dec_ifa_flags(V) -> V.
enc_ifa_flags(secondary) -> 0;
enc_ifa_flags(nodad) -> 1;
enc_ifa_flags(optimistic) -> 2;
enc_ifa_flags(dadfailed) -> 3;
enc_ifa_flags(homeaddress) -> 4;
enc_ifa_flags(deprecated) -> 5;
enc_ifa_flags(tentative) -> 6;
enc_ifa_flags(permanent) -> 7;
enc_ifa_flags(V) when is_integer(V) -> V;
enc_ifa_flags(E) -> erlang:error({undefined,E}).
dec_ctm_msgtype_ctnetlink_exp(0) -> new;
dec_ctm_msgtype_ctnetlink_exp(1) -> get;
dec_ctm_msgtype_ctnetlink_exp(2) -> delete;
dec_ctm_msgtype_ctnetlink_exp(V) -> V.
enc_ctm_msgtype_ctnetlink_exp(new) -> 0;
enc_ctm_msgtype_ctnetlink_exp(get) -> 1;
enc_ctm_msgtype_ctnetlink_exp(delete) -> 2;
enc_ctm_msgtype_ctnetlink_exp(V) when is_integer(V) -> V;
enc_ctm_msgtype_ctnetlink_exp(E) -> erlang:error({undefined,E}).
dec_ctnetlink_tuple_proto({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_tuple_proto({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_tuple_proto({1,native,<<Xe:8/unsigned-native>>}) ->
  {num,dec_protocol(Xe)};
dec_ctnetlink_tuple_proto({1,big,<<Xe:8/unsigned-big>>}) ->
  {num,dec_protocol(Xe)};
dec_ctnetlink_tuple_proto({2,native,<<X:16/unsigned-native>>}) ->
  {src_port,X};
dec_ctnetlink_tuple_proto({2,big,<<X:16/unsigned-big>>}) ->
  {src_port,X};
dec_ctnetlink_tuple_proto({3,native,<<X:16/unsigned-native>>}) ->
  {dst_port,X};
dec_ctnetlink_tuple_proto({3,big,<<X:16/unsigned-big>>}) ->
  {dst_port,X};
dec_ctnetlink_tuple_proto({4,native,<<X:16/unsigned-native>>}) ->
  {icmp_id,X};
dec_ctnetlink_tuple_proto({4,big,<<X:16/unsigned-big>>}) ->
  {icmp_id,X};
dec_ctnetlink_tuple_proto({5,native,<<X:8/unsigned-native>>}) ->
  {icmp_type,X};
dec_ctnetlink_tuple_proto({5,big,<<X:8/unsigned-big>>}) ->
  {icmp_type,X};
dec_ctnetlink_tuple_proto({6,native,<<X:8/unsigned-native>>}) ->
  {icmp_code,X};
dec_ctnetlink_tuple_proto({6,big,<<X:8/unsigned-big>>}) ->
  {icmp_code,X};
dec_ctnetlink_tuple_proto({7,native,<<X/binary>>}) ->
  {icmpv6_id,X};
dec_ctnetlink_tuple_proto({7,big,<<X/binary>>}) ->
  {icmpv6_id,X};
dec_ctnetlink_tuple_proto({8,native,<<X/binary>>}) ->
  {icmpv6_type,X};
dec_ctnetlink_tuple_proto({8,big,<<X/binary>>}) ->
  {icmpv6_type,X};
dec_ctnetlink_tuple_proto({9,native,<<X/binary>>}) ->
  {icmpv6_code,X};
dec_ctnetlink_tuple_proto({9,big,<<X/binary>>}) ->
  {icmpv6_code,X};
dec_ctnetlink_tuple_proto({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_tuple_proto({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_tuple_proto({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_tuple_proto({num,native,X}) ->
  {1,native,<<(enc_protocol(X)):8/unsigned-native>>};
enc_ctnetlink_tuple_proto({num,big,X}) ->
  {1,big,<<(enc_protocol(X)):8/unsigned-big>>};
enc_ctnetlink_tuple_proto({src_port,native,X}) ->
  {2,native,<<X:16/unsigned-native>>};
enc_ctnetlink_tuple_proto({src_port,big,X}) ->
  {2,big,<<X:16/unsigned-big>>};
enc_ctnetlink_tuple_proto({dst_port,native,X}) ->
  {3,native,<<X:16/unsigned-native>>};
enc_ctnetlink_tuple_proto({dst_port,big,X}) ->
  {3,big,<<X:16/unsigned-big>>};
enc_ctnetlink_tuple_proto({icmp_id,native,X}) ->
  {4,native,<<X:16/unsigned-native>>};
enc_ctnetlink_tuple_proto({icmp_id,big,X}) ->
  {4,big,<<X:16/unsigned-big>>};
enc_ctnetlink_tuple_proto({icmp_type,native,X}) ->
  {5,native,<<X:8/unsigned-native>>};
enc_ctnetlink_tuple_proto({icmp_type,big,X}) ->
  {5,big,<<X:8/unsigned-big>>};
enc_ctnetlink_tuple_proto({icmp_code,native,X}) ->
  {6,native,<<X:8/unsigned-native>>};
enc_ctnetlink_tuple_proto({icmp_code,big,X}) ->
  {6,big,<<X:8/unsigned-big>>};
enc_ctnetlink_tuple_proto({icmpv6_id,native,X}) ->
  {7,native,<<X>>};
enc_ctnetlink_tuple_proto({icmpv6_id,big,X}) ->
  {7,big,<<X>>};
enc_ctnetlink_tuple_proto({icmpv6_type,native,X}) ->
  {8,native,<<X>>};
enc_ctnetlink_tuple_proto({icmpv6_type,big,X}) ->
  {8,big,<<X>>};
enc_ctnetlink_tuple_proto({icmpv6_code,native,X}) ->
  {9,native,<<X>>};
enc_ctnetlink_tuple_proto({icmpv6_code,big,X}) ->
  {9,big,<<X>>};
enc_ctnetlink_tuple_proto({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_protoinfo({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_protoinfo({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_protoinfo({1,native,<<X/binary>>}) ->
  {tcp,dec_ctnetlink_protoinfo_tcp(netlink_codec:decode_tlv(X))};
dec_ctnetlink_protoinfo({1,big,<<X/binary>>}) ->
  {tcp,dec_ctnetlink_protoinfo_tcp(netlink_codec:decode_tlv(X))};
dec_ctnetlink_protoinfo({2,native,<<X/binary>>}) ->
  {dccp,X};
dec_ctnetlink_protoinfo({2,big,<<X/binary>>}) ->
  {dccp,X};
dec_ctnetlink_protoinfo({3,native,<<X/binary>>}) ->
  {sctp,X};
dec_ctnetlink_protoinfo({3,big,<<X/binary>>}) ->
  {sctp,X};
dec_ctnetlink_protoinfo({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_protoinfo({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_protoinfo({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_protoinfo({tcp,native,X}) ->
  {1,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_protoinfo_tcp(X)))>>};
enc_ctnetlink_protoinfo({tcp,big,X}) ->
  {1,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_protoinfo_tcp(X)))>>};
enc_ctnetlink_protoinfo({dccp,native,X}) ->
  {2,native,<<X>>};
enc_ctnetlink_protoinfo({dccp,big,X}) ->
  {2,big,<<X>>};
enc_ctnetlink_protoinfo({sctp,native,X}) ->
  {3,native,<<X>>};
enc_ctnetlink_protoinfo({sctp,big,X}) ->
  {3,big,<<X>>};
enc_ctnetlink_protoinfo({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_exp_tuple_proto({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_exp_tuple_proto({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_exp_tuple_proto({1,native,<<Xe:8/unsigned-native>>}) ->
  {num,dec_protocol(Xe)};
dec_ctnetlink_exp_tuple_proto({1,big,<<Xe:8/unsigned-big>>}) ->
  {num,dec_protocol(Xe)};
dec_ctnetlink_exp_tuple_proto({2,native,<<X:16/unsigned-native>>}) ->
  {src_port,X};
dec_ctnetlink_exp_tuple_proto({2,big,<<X:16/unsigned-big>>}) ->
  {src_port,X};
dec_ctnetlink_exp_tuple_proto({3,native,<<X:16/unsigned-native>>}) ->
  {dst_port,X};
dec_ctnetlink_exp_tuple_proto({3,big,<<X:16/unsigned-big>>}) ->
  {dst_port,X};
dec_ctnetlink_exp_tuple_proto({4,native,<<X:16/unsigned-native>>}) ->
  {icmp_id,X};
dec_ctnetlink_exp_tuple_proto({4,big,<<X:16/unsigned-big>>}) ->
  {icmp_id,X};
dec_ctnetlink_exp_tuple_proto({5,native,<<X:8/unsigned-native>>}) ->
  {icmp_type,X};
dec_ctnetlink_exp_tuple_proto({5,big,<<X:8/unsigned-big>>}) ->
  {icmp_type,X};
dec_ctnetlink_exp_tuple_proto({6,native,<<X:8/unsigned-native>>}) ->
  {icmp_code,X};
dec_ctnetlink_exp_tuple_proto({6,big,<<X:8/unsigned-big>>}) ->
  {icmp_code,X};
dec_ctnetlink_exp_tuple_proto({7,native,<<X/binary>>}) ->
  {icmpv6_id,X};
dec_ctnetlink_exp_tuple_proto({7,big,<<X/binary>>}) ->
  {icmpv6_id,X};
dec_ctnetlink_exp_tuple_proto({8,native,<<X/binary>>}) ->
  {icmpv6_type,X};
dec_ctnetlink_exp_tuple_proto({8,big,<<X/binary>>}) ->
  {icmpv6_type,X};
dec_ctnetlink_exp_tuple_proto({9,native,<<X/binary>>}) ->
  {icmpv6_code,X};
dec_ctnetlink_exp_tuple_proto({9,big,<<X/binary>>}) ->
  {icmpv6_code,X};
dec_ctnetlink_exp_tuple_proto({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_exp_tuple_proto({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_exp_tuple_proto({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_exp_tuple_proto({num,native,X}) ->
  {1,native,<<(enc_protocol(X)):8/unsigned-native>>};
enc_ctnetlink_exp_tuple_proto({num,big,X}) ->
  {1,big,<<(enc_protocol(X)):8/unsigned-big>>};
enc_ctnetlink_exp_tuple_proto({src_port,native,X}) ->
  {2,native,<<X:16/unsigned-native>>};
enc_ctnetlink_exp_tuple_proto({src_port,big,X}) ->
  {2,big,<<X:16/unsigned-big>>};
enc_ctnetlink_exp_tuple_proto({dst_port,native,X}) ->
  {3,native,<<X:16/unsigned-native>>};
enc_ctnetlink_exp_tuple_proto({dst_port,big,X}) ->
  {3,big,<<X:16/unsigned-big>>};
enc_ctnetlink_exp_tuple_proto({icmp_id,native,X}) ->
  {4,native,<<X:16/unsigned-native>>};
enc_ctnetlink_exp_tuple_proto({icmp_id,big,X}) ->
  {4,big,<<X:16/unsigned-big>>};
enc_ctnetlink_exp_tuple_proto({icmp_type,native,X}) ->
  {5,native,<<X:8/unsigned-native>>};
enc_ctnetlink_exp_tuple_proto({icmp_type,big,X}) ->
  {5,big,<<X:8/unsigned-big>>};
enc_ctnetlink_exp_tuple_proto({icmp_code,native,X}) ->
  {6,native,<<X:8/unsigned-native>>};
enc_ctnetlink_exp_tuple_proto({icmp_code,big,X}) ->
  {6,big,<<X:8/unsigned-big>>};
enc_ctnetlink_exp_tuple_proto({icmpv6_id,native,X}) ->
  {7,native,<<X>>};
enc_ctnetlink_exp_tuple_proto({icmpv6_id,big,X}) ->
  {7,big,<<X>>};
enc_ctnetlink_exp_tuple_proto({icmpv6_type,native,X}) ->
  {8,native,<<X>>};
enc_ctnetlink_exp_tuple_proto({icmpv6_type,big,X}) ->
  {8,big,<<X>>};
enc_ctnetlink_exp_tuple_proto({icmpv6_code,native,X}) ->
  {9,native,<<X>>};
enc_ctnetlink_exp_tuple_proto({icmpv6_code,big,X}) ->
  {9,big,<<X>>};
enc_ctnetlink_exp_tuple_proto({I,Endian,X}) -> {I,Endian,X}.
dec_rtnetlink_link_linkinfo({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_link_linkinfo({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_link_linkinfo({1,native,<<X/binary>>}) ->
  {kind,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_rtnetlink_link_linkinfo({1,big,<<X/binary>>}) ->
  {kind,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_rtnetlink_link_linkinfo({2,native,<<X/binary>>}) ->
  {data,X};
dec_rtnetlink_link_linkinfo({2,big,<<X/binary>>}) ->
  {data,X};
dec_rtnetlink_link_linkinfo({3,native,<<X/binary>>}) ->
  {xstats,X};
dec_rtnetlink_link_linkinfo({3,big,<<X/binary>>}) ->
  {xstats,X};
dec_rtnetlink_link_linkinfo({I,_Endian,Bin}) -> {I,Bin}.
enc_rtnetlink_link_linkinfo({unspec,native,X}) ->
  {0,native,<<X>>};
enc_rtnetlink_link_linkinfo({unspec,big,X}) ->
  {0,big,<<X>>};
enc_rtnetlink_link_linkinfo({kind,native,X}) ->
  {1,native,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_rtnetlink_link_linkinfo({kind,big,X}) ->
  {1,big,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_rtnetlink_link_linkinfo({data,native,X}) ->
  {2,native,<<X>>};
enc_rtnetlink_link_linkinfo({data,big,X}) ->
  {2,big,<<X>>};
enc_rtnetlink_link_linkinfo({xstats,native,X}) ->
  {3,native,<<X>>};
enc_rtnetlink_link_linkinfo({xstats,big,X}) ->
  {3,big,<<X>>};
enc_rtnetlink_link_linkinfo({I,Endian,X}) -> {I,Endian,X}.
dec_rtnetlink_link_protinfo({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_link_protinfo({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_link_protinfo({1,native,<<Xf:32/unsigned-native>>}) ->
  {flags,netlink_codec:decode_flags(Xf,fun dec_rtnetlink_link_protinfo_flags/1)};
dec_rtnetlink_link_protinfo({1,big,<<Xf:32/unsigned-big>>}) ->
  {flags,netlink_codec:decode_flags(Xf,fun dec_rtnetlink_link_protinfo_flags/1)};
dec_rtnetlink_link_protinfo({2,native,<<X/binary>>}) ->
  {conf,[Xi || <<Xi:32/signed-native>> <= X]};
dec_rtnetlink_link_protinfo({2,big,<<X/binary>>}) ->
  {conf,[Xi || <<Xi:32/signed-big>> <= X]};
dec_rtnetlink_link_protinfo({3,native,<<X/binary>>}) ->
  {stats,[Xi || <<Xi:64/unsigned-native>> <= X]};
dec_rtnetlink_link_protinfo({3,big,<<X/binary>>}) ->
  {stats,[Xi || <<Xi:64/unsigned-big>> <= X]};
dec_rtnetlink_link_protinfo({4,native,<<X/binary>>}) ->
  {mcast,X};
dec_rtnetlink_link_protinfo({4,big,<<X/binary>>}) ->
  {mcast,X};
dec_rtnetlink_link_protinfo({5,native,<<X/binary>>}) ->
  {cacheinfo,[Xi || <<Xi:32/unsigned-native>> <= X]};
dec_rtnetlink_link_protinfo({5,big,<<X/binary>>}) ->
  {cacheinfo,[Xi || <<Xi:32/unsigned-big>> <= X]};
dec_rtnetlink_link_protinfo({6,native,<<X/binary>>}) ->
  {icmp6stats,[Xi || <<Xi:64/unsigned-native>> <= X]};
dec_rtnetlink_link_protinfo({6,big,<<X/binary>>}) ->
  {icmp6stats,[Xi || <<Xi:64/unsigned-big>> <= X]};
dec_rtnetlink_link_protinfo({I,_Endian,Bin}) -> {I,Bin}.
enc_rtnetlink_link_protinfo({unspec,native,X}) ->
  {0,native,<<X>>};
enc_rtnetlink_link_protinfo({unspec,big,X}) ->
  {0,big,<<X>>};
enc_rtnetlink_link_protinfo({flags,native,X}) ->
  {1,native,<<(netlink_codec:encode_flags(X,fun enc_rtnetlink_link_protinfo_flags/1)):32/unsigned-native>>};
enc_rtnetlink_link_protinfo({flags,big,X}) ->
  {1,big,<<(netlink_codec:encode_flags(X,fun enc_rtnetlink_link_protinfo_flags/1)):32/unsigned-big>>};
enc_rtnetlink_link_protinfo({conf,native,X}) ->
  {2,native,<<(<< <<Xi:32/signed-native>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link_protinfo({conf,big,X}) ->
  {2,big,<<(<< <<Xi:32/signed-big>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link_protinfo({stats,native,X}) ->
  {3,native,<<(<< <<Xi:64/unsigned-native>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link_protinfo({stats,big,X}) ->
  {3,big,<<(<< <<Xi:64/unsigned-big>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link_protinfo({mcast,native,X}) ->
  {4,native,<<X>>};
enc_rtnetlink_link_protinfo({mcast,big,X}) ->
  {4,big,<<X>>};
enc_rtnetlink_link_protinfo({cacheinfo,native,X}) ->
  {5,native,<<(<< <<Xi:32/unsigned-native>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link_protinfo({cacheinfo,big,X}) ->
  {5,big,<<(<< <<Xi:32/unsigned-big>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link_protinfo({icmp6stats,native,X}) ->
  {6,native,<<(<< <<Xi:64/unsigned-native>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link_protinfo({icmp6stats,big,X}) ->
  {6,big,<<(<< <<Xi:64/unsigned-big>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link_protinfo({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink({1,native,<<X/binary>>}) ->
  {tuple_orig,dec_ctnetlink_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink({1,big,<<X/binary>>}) ->
  {tuple_orig,dec_ctnetlink_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink({2,native,<<X/binary>>}) ->
  {tuple_reply,dec_ctnetlink_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink({2,big,<<X/binary>>}) ->
  {tuple_reply,dec_ctnetlink_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink({3,native,<<Xf:32/unsigned-native>>}) ->
  {status,netlink_codec:decode_flags(Xf,fun dec_ctnetlink_status/1)};
dec_ctnetlink({3,big,<<Xf:32/unsigned-big>>}) ->
  {status,netlink_codec:decode_flags(Xf,fun dec_ctnetlink_status/1)};
dec_ctnetlink({4,native,<<X/binary>>}) ->
  {protoinfo,dec_ctnetlink_protoinfo(netlink_codec:decode_tlv(X))};
dec_ctnetlink({4,big,<<X/binary>>}) ->
  {protoinfo,dec_ctnetlink_protoinfo(netlink_codec:decode_tlv(X))};
dec_ctnetlink({5,native,<<X/binary>>}) ->
  {help,dec_ctnetlink_help(netlink_codec:decode_tlv(X))};
dec_ctnetlink({5,big,<<X/binary>>}) ->
  {help,dec_ctnetlink_help(netlink_codec:decode_tlv(X))};
dec_ctnetlink({6,native,<<X/binary>>}) ->
  {nat_src,X};
dec_ctnetlink({6,big,<<X/binary>>}) ->
  {nat_src,X};
dec_ctnetlink({7,native,<<X:32/unsigned-native>>}) ->
  {timeout,X};
dec_ctnetlink({7,big,<<X:32/unsigned-big>>}) ->
  {timeout,X};
dec_ctnetlink({8,native,<<X:32/unsigned-native>>}) ->
  {mark,X};
dec_ctnetlink({8,big,<<X:32/unsigned-big>>}) ->
  {mark,X};
dec_ctnetlink({9,native,<<X/binary>>}) ->
  {counters_orig,dec_ctnetlink_counters(netlink_codec:decode_tlv(X))};
dec_ctnetlink({9,big,<<X/binary>>}) ->
  {counters_orig,dec_ctnetlink_counters(netlink_codec:decode_tlv(X))};
dec_ctnetlink({10,native,<<X/binary>>}) ->
  {counters_reply,dec_ctnetlink_counters(netlink_codec:decode_tlv(X))};
dec_ctnetlink({10,big,<<X/binary>>}) ->
  {counters_reply,dec_ctnetlink_counters(netlink_codec:decode_tlv(X))};
dec_ctnetlink({11,native,<<X:32/unsigned-native>>}) ->
  {use,X};
dec_ctnetlink({11,big,<<X:32/unsigned-big>>}) ->
  {use,X};
dec_ctnetlink({12,native,<<X:32/unsigned-native>>}) ->
  {id,X};
dec_ctnetlink({12,big,<<X:32/unsigned-big>>}) ->
  {id,X};
dec_ctnetlink({13,native,<<X/binary>>}) ->
  {nat_dst,X};
dec_ctnetlink({13,big,<<X/binary>>}) ->
  {nat_dst,X};
dec_ctnetlink({14,native,<<X/binary>>}) ->
  {tuple_master,dec_ctnetlink_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink({14,big,<<X/binary>>}) ->
  {tuple_master,dec_ctnetlink_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink({15,native,<<X/binary>>}) ->
  {nat_seq_adj_orig,dec_ctnetlink_nat_seq_adj(netlink_codec:decode_tlv(X))};
dec_ctnetlink({15,big,<<X/binary>>}) ->
  {nat_seq_adj_orig,dec_ctnetlink_nat_seq_adj(netlink_codec:decode_tlv(X))};
dec_ctnetlink({16,native,<<X/binary>>}) ->
  {nat_seq_adj_reply,dec_ctnetlink_nat_seq_adj(netlink_codec:decode_tlv(X))};
dec_ctnetlink({16,big,<<X/binary>>}) ->
  {nat_seq_adj_reply,dec_ctnetlink_nat_seq_adj(netlink_codec:decode_tlv(X))};
dec_ctnetlink({17,native,<<X:32/unsigned-native>>}) ->
  {secmark,X};
dec_ctnetlink({17,big,<<X:32/unsigned-big>>}) ->
  {secmark,X};
dec_ctnetlink({18,native,<<X:16/unsigned-native>>}) ->
  {zone,X};
dec_ctnetlink({18,big,<<X:16/unsigned-big>>}) ->
  {zone,X};
dec_ctnetlink({19,native,<<X/binary>>}) ->
  {secctx,X};
dec_ctnetlink({19,big,<<X/binary>>}) ->
  {secctx,X};
dec_ctnetlink({20,native,<<X/binary>>}) ->
  {timestamp,dec_ctnetlink_timestamp(netlink_codec:decode_tlv(X))};
dec_ctnetlink({20,big,<<X/binary>>}) ->
  {timestamp,dec_ctnetlink_timestamp(netlink_codec:decode_tlv(X))};
dec_ctnetlink({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink({tuple_orig,native,X}) ->
  {1,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_tuple(X)))>>};
enc_ctnetlink({tuple_orig,big,X}) ->
  {1,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_tuple(X)))>>};
enc_ctnetlink({tuple_reply,native,X}) ->
  {2,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_tuple(X)))>>};
enc_ctnetlink({tuple_reply,big,X}) ->
  {2,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_tuple(X)))>>};
enc_ctnetlink({status,native,X}) ->
  {3,native,<<(netlink_codec:encode_flags(X,fun enc_ctnetlink_status/1)):32/unsigned-native>>};
enc_ctnetlink({status,big,X}) ->
  {3,big,<<(netlink_codec:encode_flags(X,fun enc_ctnetlink_status/1)):32/unsigned-big>>};
enc_ctnetlink({protoinfo,native,X}) ->
  {4,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_protoinfo(X)))>>};
enc_ctnetlink({protoinfo,big,X}) ->
  {4,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_protoinfo(X)))>>};
enc_ctnetlink({help,native,X}) ->
  {5,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_help(X)))>>};
enc_ctnetlink({help,big,X}) ->
  {5,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_help(X)))>>};
enc_ctnetlink({nat_src,native,X}) ->
  {6,native,<<X>>};
enc_ctnetlink({nat_src,big,X}) ->
  {6,big,<<X>>};
enc_ctnetlink({timeout,native,X}) ->
  {7,native,<<X:32/unsigned-native>>};
enc_ctnetlink({timeout,big,X}) ->
  {7,big,<<X:32/unsigned-big>>};
enc_ctnetlink({mark,native,X}) ->
  {8,native,<<X:32/unsigned-native>>};
enc_ctnetlink({mark,big,X}) ->
  {8,big,<<X:32/unsigned-big>>};
enc_ctnetlink({counters_orig,native,X}) ->
  {9,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_counters(X)))>>};
enc_ctnetlink({counters_orig,big,X}) ->
  {9,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_counters(X)))>>};
enc_ctnetlink({counters_reply,native,X}) ->
  {10,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_counters(X)))>>};
enc_ctnetlink({counters_reply,big,X}) ->
  {10,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_counters(X)))>>};
enc_ctnetlink({use,native,X}) ->
  {11,native,<<X:32/unsigned-native>>};
enc_ctnetlink({use,big,X}) ->
  {11,big,<<X:32/unsigned-big>>};
enc_ctnetlink({id,native,X}) ->
  {12,native,<<X:32/unsigned-native>>};
enc_ctnetlink({id,big,X}) ->
  {12,big,<<X:32/unsigned-big>>};
enc_ctnetlink({nat_dst,native,X}) ->
  {13,native,<<X>>};
enc_ctnetlink({nat_dst,big,X}) ->
  {13,big,<<X>>};
enc_ctnetlink({tuple_master,native,X}) ->
  {14,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_tuple(X)))>>};
enc_ctnetlink({tuple_master,big,X}) ->
  {14,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_tuple(X)))>>};
enc_ctnetlink({nat_seq_adj_orig,native,X}) ->
  {15,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_nat_seq_adj(X)))>>};
enc_ctnetlink({nat_seq_adj_orig,big,X}) ->
  {15,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_nat_seq_adj(X)))>>};
enc_ctnetlink({nat_seq_adj_reply,native,X}) ->
  {16,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_nat_seq_adj(X)))>>};
enc_ctnetlink({nat_seq_adj_reply,big,X}) ->
  {16,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_nat_seq_adj(X)))>>};
enc_ctnetlink({secmark,native,X}) ->
  {17,native,<<X:32/unsigned-native>>};
enc_ctnetlink({secmark,big,X}) ->
  {17,big,<<X:32/unsigned-big>>};
enc_ctnetlink({zone,native,X}) ->
  {18,native,<<X:16/unsigned-native>>};
enc_ctnetlink({zone,big,X}) ->
  {18,big,<<X:16/unsigned-big>>};
enc_ctnetlink({secctx,native,X}) ->
  {19,native,<<X>>};
enc_ctnetlink({secctx,big,X}) ->
  {19,big,<<X>>};
enc_ctnetlink({timestamp,native,X}) ->
  {20,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_timestamp(X)))>>};
enc_ctnetlink({timestamp,big,X}) ->
  {20,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_timestamp(X)))>>};
enc_ctnetlink({I,Endian,X}) -> {I,Endian,X}.
dec_rtnetlink_link({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_link({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_link({1,native,<<X1,X2,X3,X4,X5,X6>>}) ->
  {address,{X1,X2,X3,X4,X5,X6}};
dec_rtnetlink_link({1,big,<<X1,X2,X3,X4,X5,X6>>}) ->
  {address,{X1,X2,X3,X4,X5,X6}};
dec_rtnetlink_link({2,native,<<X1,X2,X3,X4,X5,X6>>}) ->
  {broadcast,{X1,X2,X3,X4,X5,X6}};
dec_rtnetlink_link({2,big,<<X1,X2,X3,X4,X5,X6>>}) ->
  {broadcast,{X1,X2,X3,X4,X5,X6}};
dec_rtnetlink_link({3,native,<<X/binary>>}) ->
  {ifname,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_rtnetlink_link({3,big,<<X/binary>>}) ->
  {ifname,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_rtnetlink_link({4,native,<<X:32/unsigned-native>>}) ->
  {mtu,X};
dec_rtnetlink_link({4,big,<<X:32/unsigned-big>>}) ->
  {mtu,X};
dec_rtnetlink_link({5,native,<<X:32/unsigned-native>>}) ->
  {link,X};
dec_rtnetlink_link({5,big,<<X:32/unsigned-big>>}) ->
  {link,X};
dec_rtnetlink_link({6,native,<<X/binary>>}) ->
  {qdisc,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_rtnetlink_link({6,big,<<X/binary>>}) ->
  {qdisc,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_rtnetlink_link({7,native,<<X/binary>>}) ->
  {stats,[Xi || <<Xi:32/unsigned-native>> <= X]};
dec_rtnetlink_link({7,big,<<X/binary>>}) ->
  {stats,[Xi || <<Xi:32/unsigned-big>> <= X]};
dec_rtnetlink_link({8,native,<<X/binary>>}) ->
  {cost,X};
dec_rtnetlink_link({8,big,<<X/binary>>}) ->
  {cost,X};
dec_rtnetlink_link({9,native,<<X/binary>>}) ->
  {priority,X};
dec_rtnetlink_link({9,big,<<X/binary>>}) ->
  {priority,X};
dec_rtnetlink_link({10,native,<<X/binary>>}) ->
  {master,X};
dec_rtnetlink_link({10,big,<<X/binary>>}) ->
  {master,X};
dec_rtnetlink_link({11,native,<<X/binary>>}) ->
  {wireless,X};
dec_rtnetlink_link({11,big,<<X/binary>>}) ->
  {wireless,X};
dec_rtnetlink_link({12,native,<<X/binary>>}) ->
  {protinfo,[dec_rtnetlink_link_protinfo(Xi) || Xi <- netlink_codec:decode_tlv_list(X)]};
dec_rtnetlink_link({12,big,<<X/binary>>}) ->
  {protinfo,[dec_rtnetlink_link_protinfo(Xi) || Xi <- netlink_codec:decode_tlv_list(X)]};
dec_rtnetlink_link({13,native,<<X:32/unsigned-native>>}) ->
  {txqlen,X};
dec_rtnetlink_link({13,big,<<X:32/unsigned-big>>}) ->
  {txqlen,X};
dec_rtnetlink_link({14,native,<<X1:64/unsigned-native,X2:64/unsigned-native,X3:64/unsigned-native,X4:16/unsigned-native,X5:8/unsigned-native,X6:8/unsigned-native>>}) ->
  {map,#if_map{memstart=X1,memend=X2,baseaddr=X3,irq=X4,dma=X5,port=X6}};
dec_rtnetlink_link({14,big,<<X1:64/unsigned-big,X2:64/unsigned-big,X3:64/unsigned-big,X4:16/unsigned-big,X5:8/unsigned-big,X6:8/unsigned-big>>}) ->
  {map,#if_map{memstart=X1,memend=X2,baseaddr=X3,irq=X4,dma=X5,port=X6}};
dec_rtnetlink_link({15,native,<<X/binary>>}) ->
  {weight,X};
dec_rtnetlink_link({15,big,<<X/binary>>}) ->
  {weight,X};
dec_rtnetlink_link({16,native,<<Xe:8/unsigned-native>>}) ->
  {operstate,dec_rtnetlink_link_operstate(Xe)};
dec_rtnetlink_link({16,big,<<Xe:8/unsigned-big>>}) ->
  {operstate,dec_rtnetlink_link_operstate(Xe)};
dec_rtnetlink_link({17,native,<<Xe:8/unsigned-native>>}) ->
  {linkmode,dec_rtnetlink_link_linkmode(Xe)};
dec_rtnetlink_link({17,big,<<Xe:8/unsigned-big>>}) ->
  {linkmode,dec_rtnetlink_link_linkmode(Xe)};
dec_rtnetlink_link({18,native,<<X/binary>>}) ->
  {linkinfo,[dec_rtnetlink_link_linkinfo(Xi) || Xi <- netlink_codec:decode_tlv_list(X)]};
dec_rtnetlink_link({18,big,<<X/binary>>}) ->
  {linkinfo,[dec_rtnetlink_link_linkinfo(Xi) || Xi <- netlink_codec:decode_tlv_list(X)]};
dec_rtnetlink_link({19,native,<<X/binary>>}) ->
  {net_ns_pid,X};
dec_rtnetlink_link({19,big,<<X/binary>>}) ->
  {net_ns_pid,X};
dec_rtnetlink_link({20,native,<<X/binary>>}) ->
  {ifalias,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_rtnetlink_link({20,big,<<X/binary>>}) ->
  {ifalias,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_rtnetlink_link({21,native,<<X:32/unsigned-native>>}) ->
  {num_vf,X};
dec_rtnetlink_link({21,big,<<X:32/unsigned-big>>}) ->
  {num_vf,X};
dec_rtnetlink_link({22,native,<<X/binary>>}) ->
  {vfinfo_list,X};
dec_rtnetlink_link({22,big,<<X/binary>>}) ->
  {vfinfo_list,X};
dec_rtnetlink_link({23,native,<<X/binary>>}) ->
  {stats64,[Xi || <<Xi:64/unsigned-native>> <= X]};
dec_rtnetlink_link({23,big,<<X/binary>>}) ->
  {stats64,[Xi || <<Xi:64/unsigned-big>> <= X]};
dec_rtnetlink_link({24,native,<<X/binary>>}) ->
  {vf_ports,X};
dec_rtnetlink_link({24,big,<<X/binary>>}) ->
  {vf_ports,X};
dec_rtnetlink_link({25,native,<<X/binary>>}) ->
  {port_self,X};
dec_rtnetlink_link({25,big,<<X/binary>>}) ->
  {port_self,X};
dec_rtnetlink_link({26,native,<<X/binary>>}) ->
  {af_spec,X};
dec_rtnetlink_link({26,big,<<X/binary>>}) ->
  {af_spec,X};
dec_rtnetlink_link({27,native,<<X:32/unsigned-native>>}) ->
  {group,X};
dec_rtnetlink_link({27,big,<<X:32/unsigned-big>>}) ->
  {group,X};
dec_rtnetlink_link({28,native,<<X/binary>>}) ->
  {net_ns_fd,X};
dec_rtnetlink_link({28,big,<<X/binary>>}) ->
  {net_ns_fd,X};
dec_rtnetlink_link({29,native,<<X/binary>>}) ->
  {ext_mask,X};
dec_rtnetlink_link({29,big,<<X/binary>>}) ->
  {ext_mask,X};
dec_rtnetlink_link({30,native,<<X:32/unsigned-native>>}) ->
  {promiscuity,X};
dec_rtnetlink_link({30,big,<<X:32/unsigned-big>>}) ->
  {promiscuity,X};
dec_rtnetlink_link({I,_Endian,Bin}) -> {I,Bin}.
enc_rtnetlink_link({unspec,native,X}) ->
  {0,native,<<X>>};
enc_rtnetlink_link({unspec,big,X}) ->
  {0,big,<<X>>};
enc_rtnetlink_link({address,native,{X1,X2,X3,X4,X5,X6}}) ->
  {1,native,<<X1:8,X2:8,X3:8,X4:8,X5:8,X6:8>>};
enc_rtnetlink_link({address,big,{X1,X2,X3,X4,X5,X6}}) ->
  {1,big,<<X1:8,X2:8,X3:8,X4:8,X5:8,X6:8>>};
enc_rtnetlink_link({broadcast,native,{X1,X2,X3,X4,X5,X6}}) ->
  {2,native,<<X1:8,X2:8,X3:8,X4:8,X5:8,X6:8>>};
enc_rtnetlink_link({broadcast,big,{X1,X2,X3,X4,X5,X6}}) ->
  {2,big,<<X1:8,X2:8,X3:8,X4:8,X5:8,X6:8>>};
enc_rtnetlink_link({ifname,native,X}) ->
  {3,native,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_rtnetlink_link({ifname,big,X}) ->
  {3,big,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_rtnetlink_link({mtu,native,X}) ->
  {4,native,<<X:32/unsigned-native>>};
enc_rtnetlink_link({mtu,big,X}) ->
  {4,big,<<X:32/unsigned-big>>};
enc_rtnetlink_link({link,native,X}) ->
  {5,native,<<X:32/unsigned-native>>};
enc_rtnetlink_link({link,big,X}) ->
  {5,big,<<X:32/unsigned-big>>};
enc_rtnetlink_link({qdisc,native,X}) ->
  {6,native,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_rtnetlink_link({qdisc,big,X}) ->
  {6,big,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_rtnetlink_link({stats,native,X}) ->
  {7,native,<<(<< <<Xi:32/unsigned-native>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link({stats,big,X}) ->
  {7,big,<<(<< <<Xi:32/unsigned-big>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link({cost,native,X}) ->
  {8,native,<<X>>};
enc_rtnetlink_link({cost,big,X}) ->
  {8,big,<<X>>};
enc_rtnetlink_link({priority,native,X}) ->
  {9,native,<<X>>};
enc_rtnetlink_link({priority,big,X}) ->
  {9,big,<<X>>};
enc_rtnetlink_link({master,native,X}) ->
  {10,native,<<X>>};
enc_rtnetlink_link({master,big,X}) ->
  {10,big,<<X>>};
enc_rtnetlink_link({wireless,native,X}) ->
  {11,native,<<X>>};
enc_rtnetlink_link({wireless,big,X}) ->
  {11,big,<<X>>};
enc_rtnetlink_link({protinfo,native,X}) ->
  {12,native,<<(netlink_codec:encode_tlv_list([enc_rtnetlink_link_protinfo(Xi) || Xi <- X]))/binary>>};
enc_rtnetlink_link({protinfo,big,X}) ->
  {12,big,<<(netlink_codec:encode_tlv_list([enc_rtnetlink_link_protinfo(Xi) || Xi <- X]))/binary>>};
enc_rtnetlink_link({txqlen,native,X}) ->
  {13,native,<<X:32/unsigned-native>>};
enc_rtnetlink_link({txqlen,big,X}) ->
  {13,big,<<X:32/unsigned-big>>};
enc_rtnetlink_link({map,native,#if_map{memstart=X1,memend=X2,baseaddr=X3,irq=X4,dma=X5,port=X6}}) ->
  {14,native,<<X1:64/unsigned-native,X2:64/unsigned-native,X3:64/unsigned-native,X4:16/unsigned-native,X5:8/unsigned-native,X6:8/unsigned-native>>};
enc_rtnetlink_link({map,big,#if_map{memstart=X1,memend=X2,baseaddr=X3,irq=X4,dma=X5,port=X6}}) ->
  {14,big,<<X1:64/unsigned-big,X2:64/unsigned-big,X3:64/unsigned-big,X4:16/unsigned-big,X5:8/unsigned-big,X6:8/unsigned-big>>};
enc_rtnetlink_link({weight,native,X}) ->
  {15,native,<<X>>};
enc_rtnetlink_link({weight,big,X}) ->
  {15,big,<<X>>};
enc_rtnetlink_link({operstate,native,X}) ->
  {16,native,<<(enc_rtnetlink_link_operstate(X)):8/unsigned-native>>};
enc_rtnetlink_link({operstate,big,X}) ->
  {16,big,<<(enc_rtnetlink_link_operstate(X)):8/unsigned-big>>};
enc_rtnetlink_link({linkmode,native,X}) ->
  {17,native,<<(enc_rtnetlink_link_linkmode(X)):8/unsigned-native>>};
enc_rtnetlink_link({linkmode,big,X}) ->
  {17,big,<<(enc_rtnetlink_link_linkmode(X)):8/unsigned-big>>};
enc_rtnetlink_link({linkinfo,native,X}) ->
  {18,native,<<(netlink_codec:encode_tlv_list([enc_rtnetlink_link_linkinfo(Xi) || Xi <- X]))/binary>>};
enc_rtnetlink_link({linkinfo,big,X}) ->
  {18,big,<<(netlink_codec:encode_tlv_list([enc_rtnetlink_link_linkinfo(Xi) || Xi <- X]))/binary>>};
enc_rtnetlink_link({net_ns_pid,native,X}) ->
  {19,native,<<X>>};
enc_rtnetlink_link({net_ns_pid,big,X}) ->
  {19,big,<<X>>};
enc_rtnetlink_link({ifalias,native,X}) ->
  {20,native,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_rtnetlink_link({ifalias,big,X}) ->
  {20,big,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_rtnetlink_link({num_vf,native,X}) ->
  {21,native,<<X:32/unsigned-native>>};
enc_rtnetlink_link({num_vf,big,X}) ->
  {21,big,<<X:32/unsigned-big>>};
enc_rtnetlink_link({vfinfo_list,native,X}) ->
  {22,native,<<X>>};
enc_rtnetlink_link({vfinfo_list,big,X}) ->
  {22,big,<<X>>};
enc_rtnetlink_link({stats64,native,X}) ->
  {23,native,<<(<< <<Xi:64/unsigned-native>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link({stats64,big,X}) ->
  {23,big,<<(<< <<Xi:64/unsigned-big>> || Xi <- X>>)/binary>>};
enc_rtnetlink_link({vf_ports,native,X}) ->
  {24,native,<<X>>};
enc_rtnetlink_link({vf_ports,big,X}) ->
  {24,big,<<X>>};
enc_rtnetlink_link({port_self,native,X}) ->
  {25,native,<<X>>};
enc_rtnetlink_link({port_self,big,X}) ->
  {25,big,<<X>>};
enc_rtnetlink_link({af_spec,native,X}) ->
  {26,native,<<X>>};
enc_rtnetlink_link({af_spec,big,X}) ->
  {26,big,<<X>>};
enc_rtnetlink_link({group,native,X}) ->
  {27,native,<<X:32/unsigned-native>>};
enc_rtnetlink_link({group,big,X}) ->
  {27,big,<<X:32/unsigned-big>>};
enc_rtnetlink_link({net_ns_fd,native,X}) ->
  {28,native,<<X>>};
enc_rtnetlink_link({net_ns_fd,big,X}) ->
  {28,big,<<X>>};
enc_rtnetlink_link({ext_mask,native,X}) ->
  {29,native,<<X>>};
enc_rtnetlink_link({ext_mask,big,X}) ->
  {29,big,<<X>>};
enc_rtnetlink_link({promiscuity,native,X}) ->
  {30,native,<<X:32/unsigned-native>>};
enc_rtnetlink_link({promiscuity,big,X}) ->
  {30,big,<<X:32/unsigned-big>>};
enc_rtnetlink_link({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_nat_seq_adj({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_nat_seq_adj({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_nat_seq_adj({1,native,<<X:32/unsigned-native>>}) ->
  {correction_pos,X};
dec_ctnetlink_nat_seq_adj({1,big,<<X:32/unsigned-big>>}) ->
  {correction_pos,X};
dec_ctnetlink_nat_seq_adj({2,native,<<X:32/unsigned-native>>}) ->
  {offset_before,X};
dec_ctnetlink_nat_seq_adj({2,big,<<X:32/unsigned-big>>}) ->
  {offset_before,X};
dec_ctnetlink_nat_seq_adj({3,native,<<X:32/unsigned-native>>}) ->
  {offset_after,X};
dec_ctnetlink_nat_seq_adj({3,big,<<X:32/unsigned-big>>}) ->
  {offset_after,X};
dec_ctnetlink_nat_seq_adj({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_nat_seq_adj({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_nat_seq_adj({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_nat_seq_adj({correction_pos,native,X}) ->
  {1,native,<<X:32/unsigned-native>>};
enc_ctnetlink_nat_seq_adj({correction_pos,big,X}) ->
  {1,big,<<X:32/unsigned-big>>};
enc_ctnetlink_nat_seq_adj({offset_before,native,X}) ->
  {2,native,<<X:32/unsigned-native>>};
enc_ctnetlink_nat_seq_adj({offset_before,big,X}) ->
  {2,big,<<X:32/unsigned-big>>};
enc_ctnetlink_nat_seq_adj({offset_after,native,X}) ->
  {3,native,<<X:32/unsigned-native>>};
enc_ctnetlink_nat_seq_adj({offset_after,big,X}) ->
  {3,big,<<X:32/unsigned-big>>};
enc_ctnetlink_nat_seq_adj({I,Endian,X}) -> {I,Endian,X}.
dec_rtnetlink_neigh({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_neigh({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_neigh({1,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {dst,{X1,X2,X3,X4}};
dec_rtnetlink_neigh({1,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {dst,{X1,X2,X3,X4}};
dec_rtnetlink_neigh({1,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {dst,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_neigh({1,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {dst,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_neigh({2,native,<<X1,X2,X3,X4,X5,X6>>}) ->
  {lladdr,{X1,X2,X3,X4,X5,X6}};
dec_rtnetlink_neigh({2,big,<<X1,X2,X3,X4,X5,X6>>}) ->
  {lladdr,{X1,X2,X3,X4,X5,X6}};
dec_rtnetlink_neigh({3,native,<<X/binary>>}) ->
  {cacheinfo,[Xi || <<Xi:32/unsigned-native>> <= X]};
dec_rtnetlink_neigh({3,big,<<X/binary>>}) ->
  {cacheinfo,[Xi || <<Xi:32/unsigned-big>> <= X]};
dec_rtnetlink_neigh({4,native,<<X:32/unsigned-native>>}) ->
  {probes,X};
dec_rtnetlink_neigh({4,big,<<X:32/unsigned-big>>}) ->
  {probes,X};
dec_rtnetlink_neigh({I,_Endian,Bin}) -> {I,Bin}.
enc_rtnetlink_neigh({unspec,native,X}) ->
  {0,native,<<X>>};
enc_rtnetlink_neigh({unspec,big,X}) ->
  {0,big,<<X>>};
enc_rtnetlink_neigh({dst,native,{X1,X2,X3,X4}}) ->
  {1,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_neigh({dst,big,{X1,X2,X3,X4}}) ->
  {1,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_neigh({dst,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {1,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_neigh({dst,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {1,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_neigh({lladdr,native,{X1,X2,X3,X4,X5,X6}}) ->
  {2,native,<<X1:8,X2:8,X3:8,X4:8,X5:8,X6:8>>};
enc_rtnetlink_neigh({lladdr,big,{X1,X2,X3,X4,X5,X6}}) ->
  {2,big,<<X1:8,X2:8,X3:8,X4:8,X5:8,X6:8>>};
enc_rtnetlink_neigh({cacheinfo,native,X}) ->
  {3,native,<<(<< <<Xi:32/unsigned-native>> || Xi <- X>>)/binary>>};
enc_rtnetlink_neigh({cacheinfo,big,X}) ->
  {3,big,<<(<< <<Xi:32/unsigned-big>> || Xi <- X>>)/binary>>};
enc_rtnetlink_neigh({probes,native,X}) ->
  {4,native,<<X:32/unsigned-native>>};
enc_rtnetlink_neigh({probes,big,X}) ->
  {4,big,<<X:32/unsigned-big>>};
enc_rtnetlink_neigh({I,Endian,X}) -> {I,Endian,X}.
dec_rtnetlink_prefix({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_prefix({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_prefix({1,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {address,{X1,X2,X3,X4}};
dec_rtnetlink_prefix({1,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {address,{X1,X2,X3,X4}};
dec_rtnetlink_prefix({1,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {address,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_prefix({1,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {address,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_prefix({2,native,<<X/binary>>}) ->
  {cacheinfo,[Xi || <<Xi:32/unsigned-native>> <= X]};
dec_rtnetlink_prefix({2,big,<<X/binary>>}) ->
  {cacheinfo,[Xi || <<Xi:32/unsigned-big>> <= X]};
dec_rtnetlink_prefix({I,_Endian,Bin}) -> {I,Bin}.
enc_rtnetlink_prefix({unspec,native,X}) ->
  {0,native,<<X>>};
enc_rtnetlink_prefix({unspec,big,X}) ->
  {0,big,<<X>>};
enc_rtnetlink_prefix({address,native,{X1,X2,X3,X4}}) ->
  {1,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_prefix({address,big,{X1,X2,X3,X4}}) ->
  {1,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_prefix({address,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {1,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_prefix({address,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {1,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_prefix({cacheinfo,native,X}) ->
  {2,native,<<(<< <<Xi:32/unsigned-native>> || Xi <- X>>)/binary>>};
enc_rtnetlink_prefix({cacheinfo,big,X}) ->
  {2,big,<<(<< <<Xi:32/unsigned-big>> || Xi <- X>>)/binary>>};
enc_rtnetlink_prefix({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_tuple({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_tuple({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_tuple({1,native,<<X/binary>>}) ->
  {ip,dec_ctnetlink_tuple_ip(netlink_codec:decode_tlv(X))};
dec_ctnetlink_tuple({1,big,<<X/binary>>}) ->
  {ip,dec_ctnetlink_tuple_ip(netlink_codec:decode_tlv(X))};
dec_ctnetlink_tuple({2,native,<<X/binary>>}) ->
  {proto,dec_ctnetlink_tuple_proto(netlink_codec:decode_tlv(X))};
dec_ctnetlink_tuple({2,big,<<X/binary>>}) ->
  {proto,dec_ctnetlink_tuple_proto(netlink_codec:decode_tlv(X))};
dec_ctnetlink_tuple({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_tuple({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_tuple({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_tuple({ip,native,X}) ->
  {1,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_tuple_ip(X)))>>};
enc_ctnetlink_tuple({ip,big,X}) ->
  {1,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_tuple_ip(X)))>>};
enc_ctnetlink_tuple({proto,native,X}) ->
  {2,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_tuple_proto(X)))>>};
enc_ctnetlink_tuple({proto,big,X}) ->
  {2,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_tuple_proto(X)))>>};
enc_ctnetlink_tuple({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_exp_tuple({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_exp_tuple({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_exp_tuple({1,native,<<X/binary>>}) ->
  {ip,dec_ctnetlink_exp_tuple_ip(netlink_codec:decode_tlv(X))};
dec_ctnetlink_exp_tuple({1,big,<<X/binary>>}) ->
  {ip,dec_ctnetlink_exp_tuple_ip(netlink_codec:decode_tlv(X))};
dec_ctnetlink_exp_tuple({2,native,<<X/binary>>}) ->
  {proto,dec_ctnetlink_exp_tuple_proto(netlink_codec:decode_tlv(X))};
dec_ctnetlink_exp_tuple({2,big,<<X/binary>>}) ->
  {proto,dec_ctnetlink_exp_tuple_proto(netlink_codec:decode_tlv(X))};
dec_ctnetlink_exp_tuple({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_exp_tuple({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_exp_tuple({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_exp_tuple({ip,native,X}) ->
  {1,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_exp_tuple_ip(X)))>>};
enc_ctnetlink_exp_tuple({ip,big,X}) ->
  {1,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_exp_tuple_ip(X)))>>};
enc_ctnetlink_exp_tuple({proto,native,X}) ->
  {2,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_exp_tuple_proto(X)))>>};
enc_ctnetlink_exp_tuple({proto,big,X}) ->
  {2,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_exp_tuple_proto(X)))>>};
enc_ctnetlink_exp_tuple({I,Endian,X}) -> {I,Endian,X}.
dec_rtnetlink_route({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_route({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_route({1,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {dst,{X1,X2,X3,X4}};
dec_rtnetlink_route({1,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {dst,{X1,X2,X3,X4}};
dec_rtnetlink_route({1,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {dst,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_route({1,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {dst,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_route({2,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {src,{X1,X2,X3,X4}};
dec_rtnetlink_route({2,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {src,{X1,X2,X3,X4}};
dec_rtnetlink_route({2,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {src,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_route({2,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {src,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_route({3,native,<<X:32/unsigned-native>>}) ->
  {iif,X};
dec_rtnetlink_route({3,big,<<X:32/unsigned-big>>}) ->
  {iif,X};
dec_rtnetlink_route({4,native,<<X:32/unsigned-native>>}) ->
  {oif,X};
dec_rtnetlink_route({4,big,<<X:32/unsigned-big>>}) ->
  {oif,X};
dec_rtnetlink_route({5,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {gateway,{X1,X2,X3,X4}};
dec_rtnetlink_route({5,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {gateway,{X1,X2,X3,X4}};
dec_rtnetlink_route({5,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {gateway,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_route({5,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {gateway,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_route({6,native,<<X:32/unsigned-native>>}) ->
  {priority,X};
dec_rtnetlink_route({6,big,<<X:32/unsigned-big>>}) ->
  {priority,X};
dec_rtnetlink_route({7,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {prefsrc,{X1,X2,X3,X4}};
dec_rtnetlink_route({7,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {prefsrc,{X1,X2,X3,X4}};
dec_rtnetlink_route({7,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {prefsrc,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_route({7,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {prefsrc,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_route({8,native,<<X/binary>>}) ->
  {metrics,[dec_rtnetlink_route_metrics(Xi) || Xi <- netlink_codec:decode_tlv_list(X)]};
dec_rtnetlink_route({8,big,<<X/binary>>}) ->
  {metrics,[dec_rtnetlink_route_metrics(Xi) || Xi <- netlink_codec:decode_tlv_list(X)]};
dec_rtnetlink_route({9,native,<<X/binary>>}) ->
  {multipath,X};
dec_rtnetlink_route({9,big,<<X/binary>>}) ->
  {multipath,X};
dec_rtnetlink_route({10,native,<<X/binary>>}) ->
  {protoinfo,X};
dec_rtnetlink_route({10,big,<<X/binary>>}) ->
  {protoinfo,X};
dec_rtnetlink_route({11,native,<<X:32/unsigned-native>>}) ->
  {flow,X};
dec_rtnetlink_route({11,big,<<X:32/unsigned-big>>}) ->
  {flow,X};
dec_rtnetlink_route({12,native,<<X/binary>>}) ->
  {cacheinfo,[Xi || <<Xi:32/unsigned-native>> <= X]};
dec_rtnetlink_route({12,big,<<X/binary>>}) ->
  {cacheinfo,[Xi || <<Xi:32/unsigned-big>> <= X]};
dec_rtnetlink_route({13,native,<<X/binary>>}) ->
  {session,X};
dec_rtnetlink_route({13,big,<<X/binary>>}) ->
  {session,X};
dec_rtnetlink_route({14,native,<<X/binary>>}) ->
  {mp_algo,X};
dec_rtnetlink_route({14,big,<<X/binary>>}) ->
  {mp_algo,X};
dec_rtnetlink_route({15,native,<<X:32/unsigned-native>>}) ->
  {table,X};
dec_rtnetlink_route({15,big,<<X:32/unsigned-big>>}) ->
  {table,X};
dec_rtnetlink_route({I,_Endian,Bin}) -> {I,Bin}.
enc_rtnetlink_route({unspec,native,X}) ->
  {0,native,<<X>>};
enc_rtnetlink_route({unspec,big,X}) ->
  {0,big,<<X>>};
enc_rtnetlink_route({dst,native,{X1,X2,X3,X4}}) ->
  {1,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_route({dst,big,{X1,X2,X3,X4}}) ->
  {1,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_route({dst,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {1,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_route({dst,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {1,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_route({src,native,{X1,X2,X3,X4}}) ->
  {2,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_route({src,big,{X1,X2,X3,X4}}) ->
  {2,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_route({src,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {2,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_route({src,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {2,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_route({iif,native,X}) ->
  {3,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route({iif,big,X}) ->
  {3,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route({oif,native,X}) ->
  {4,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route({oif,big,X}) ->
  {4,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route({gateway,native,{X1,X2,X3,X4}}) ->
  {5,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_route({gateway,big,{X1,X2,X3,X4}}) ->
  {5,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_route({gateway,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {5,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_route({gateway,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {5,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_route({priority,native,X}) ->
  {6,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route({priority,big,X}) ->
  {6,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route({prefsrc,native,{X1,X2,X3,X4}}) ->
  {7,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_route({prefsrc,big,{X1,X2,X3,X4}}) ->
  {7,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_route({prefsrc,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {7,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_route({prefsrc,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {7,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_route({metrics,native,X}) ->
  {8,native,<<(netlink_codec:encode_tlv_list([enc_rtnetlink_route_metrics(Xi) || Xi <- X]))/binary>>};
enc_rtnetlink_route({metrics,big,X}) ->
  {8,big,<<(netlink_codec:encode_tlv_list([enc_rtnetlink_route_metrics(Xi) || Xi <- X]))/binary>>};
enc_rtnetlink_route({multipath,native,X}) ->
  {9,native,<<X>>};
enc_rtnetlink_route({multipath,big,X}) ->
  {9,big,<<X>>};
enc_rtnetlink_route({protoinfo,native,X}) ->
  {10,native,<<X>>};
enc_rtnetlink_route({protoinfo,big,X}) ->
  {10,big,<<X>>};
enc_rtnetlink_route({flow,native,X}) ->
  {11,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route({flow,big,X}) ->
  {11,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route({cacheinfo,native,X}) ->
  {12,native,<<(<< <<Xi:32/unsigned-native>> || Xi <- X>>)/binary>>};
enc_rtnetlink_route({cacheinfo,big,X}) ->
  {12,big,<<(<< <<Xi:32/unsigned-big>> || Xi <- X>>)/binary>>};
enc_rtnetlink_route({session,native,X}) ->
  {13,native,<<X>>};
enc_rtnetlink_route({session,big,X}) ->
  {13,big,<<X>>};
enc_rtnetlink_route({mp_algo,native,X}) ->
  {14,native,<<X>>};
enc_rtnetlink_route({mp_algo,big,X}) ->
  {14,big,<<X>>};
enc_rtnetlink_route({table,native,X}) ->
  {15,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route({table,big,X}) ->
  {15,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_counters({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_counters({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_counters({1,native,<<X:64/unsigned-native>>}) ->
  {packets,X};
dec_ctnetlink_counters({1,big,<<X:64/unsigned-big>>}) ->
  {packets,X};
dec_ctnetlink_counters({2,native,<<X:64/unsigned-native>>}) ->
  {bytes,X};
dec_ctnetlink_counters({2,big,<<X:64/unsigned-big>>}) ->
  {bytes,X};
dec_ctnetlink_counters({3,native,<<X:32/unsigned-native>>}) ->
  {packets32,X};
dec_ctnetlink_counters({3,big,<<X:32/unsigned-big>>}) ->
  {packets32,X};
dec_ctnetlink_counters({4,native,<<X:32/unsigned-native>>}) ->
  {bytes32,X};
dec_ctnetlink_counters({4,big,<<X:32/unsigned-big>>}) ->
  {bytes32,X};
dec_ctnetlink_counters({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_counters({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_counters({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_counters({packets,native,X}) ->
  {1,native,<<X:64/unsigned-native>>};
enc_ctnetlink_counters({packets,big,X}) ->
  {1,big,<<X:64/unsigned-big>>};
enc_ctnetlink_counters({bytes,native,X}) ->
  {2,native,<<X:64/unsigned-native>>};
enc_ctnetlink_counters({bytes,big,X}) ->
  {2,big,<<X:64/unsigned-big>>};
enc_ctnetlink_counters({packets32,native,X}) ->
  {3,native,<<X:32/unsigned-native>>};
enc_ctnetlink_counters({packets32,big,X}) ->
  {3,big,<<X:32/unsigned-big>>};
enc_ctnetlink_counters({bytes32,native,X}) ->
  {4,native,<<X:32/unsigned-native>>};
enc_ctnetlink_counters({bytes32,big,X}) ->
  {4,big,<<X:32/unsigned-big>>};
enc_ctnetlink_counters({I,Endian,X}) -> {I,Endian,X}.
dec_rtnetlink_route_metrics({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_route_metrics({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_route_metrics({1,native,<<X:32/unsigned-native>>}) ->
  {lock,X};
dec_rtnetlink_route_metrics({1,big,<<X:32/unsigned-big>>}) ->
  {lock,X};
dec_rtnetlink_route_metrics({2,native,<<X:32/unsigned-native>>}) ->
  {mtu,X};
dec_rtnetlink_route_metrics({2,big,<<X:32/unsigned-big>>}) ->
  {mtu,X};
dec_rtnetlink_route_metrics({3,native,<<X:32/unsigned-native>>}) ->
  {window,X};
dec_rtnetlink_route_metrics({3,big,<<X:32/unsigned-big>>}) ->
  {window,X};
dec_rtnetlink_route_metrics({4,native,<<X:32/unsigned-native>>}) ->
  {rtt,X};
dec_rtnetlink_route_metrics({4,big,<<X:32/unsigned-big>>}) ->
  {rtt,X};
dec_rtnetlink_route_metrics({5,native,<<X:32/unsigned-native>>}) ->
  {rttvar,X};
dec_rtnetlink_route_metrics({5,big,<<X:32/unsigned-big>>}) ->
  {rttvar,X};
dec_rtnetlink_route_metrics({6,native,<<X:32/unsigned-native>>}) ->
  {ssthresh,X};
dec_rtnetlink_route_metrics({6,big,<<X:32/unsigned-big>>}) ->
  {ssthresh,X};
dec_rtnetlink_route_metrics({7,native,<<X:32/unsigned-native>>}) ->
  {cwnd,X};
dec_rtnetlink_route_metrics({7,big,<<X:32/unsigned-big>>}) ->
  {cwnd,X};
dec_rtnetlink_route_metrics({8,native,<<X:32/unsigned-native>>}) ->
  {advmss,X};
dec_rtnetlink_route_metrics({8,big,<<X:32/unsigned-big>>}) ->
  {advmss,X};
dec_rtnetlink_route_metrics({9,native,<<X:32/unsigned-native>>}) ->
  {reordering,X};
dec_rtnetlink_route_metrics({9,big,<<X:32/unsigned-big>>}) ->
  {reordering,X};
dec_rtnetlink_route_metrics({10,native,<<X:32/unsigned-native>>}) ->
  {hoplimit,X};
dec_rtnetlink_route_metrics({10,big,<<X:32/unsigned-big>>}) ->
  {hoplimit,X};
dec_rtnetlink_route_metrics({11,native,<<X:32/unsigned-native>>}) ->
  {initcwnd,X};
dec_rtnetlink_route_metrics({11,big,<<X:32/unsigned-big>>}) ->
  {initcwnd,X};
dec_rtnetlink_route_metrics({12,native,<<X:32/unsigned-native>>}) ->
  {features,X};
dec_rtnetlink_route_metrics({12,big,<<X:32/unsigned-big>>}) ->
  {features,X};
dec_rtnetlink_route_metrics({13,native,<<X:32/unsigned-native>>}) ->
  {rto_min,X};
dec_rtnetlink_route_metrics({13,big,<<X:32/unsigned-big>>}) ->
  {rto_min,X};
dec_rtnetlink_route_metrics({14,native,<<X:32/unsigned-native>>}) ->
  {initrwnd,X};
dec_rtnetlink_route_metrics({14,big,<<X:32/unsigned-big>>}) ->
  {initrwnd,X};
dec_rtnetlink_route_metrics({I,_Endian,Bin}) -> {I,Bin}.
enc_rtnetlink_route_metrics({unspec,native,X}) ->
  {0,native,<<X>>};
enc_rtnetlink_route_metrics({unspec,big,X}) ->
  {0,big,<<X>>};
enc_rtnetlink_route_metrics({lock,native,X}) ->
  {1,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({lock,big,X}) ->
  {1,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({mtu,native,X}) ->
  {2,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({mtu,big,X}) ->
  {2,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({window,native,X}) ->
  {3,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({window,big,X}) ->
  {3,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({rtt,native,X}) ->
  {4,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({rtt,big,X}) ->
  {4,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({rttvar,native,X}) ->
  {5,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({rttvar,big,X}) ->
  {5,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({ssthresh,native,X}) ->
  {6,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({ssthresh,big,X}) ->
  {6,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({cwnd,native,X}) ->
  {7,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({cwnd,big,X}) ->
  {7,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({advmss,native,X}) ->
  {8,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({advmss,big,X}) ->
  {8,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({reordering,native,X}) ->
  {9,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({reordering,big,X}) ->
  {9,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({hoplimit,native,X}) ->
  {10,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({hoplimit,big,X}) ->
  {10,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({initcwnd,native,X}) ->
  {11,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({initcwnd,big,X}) ->
  {11,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({features,native,X}) ->
  {12,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({features,big,X}) ->
  {12,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({rto_min,native,X}) ->
  {13,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({rto_min,big,X}) ->
  {13,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({initrwnd,native,X}) ->
  {14,native,<<X:32/unsigned-native>>};
enc_rtnetlink_route_metrics({initrwnd,big,X}) ->
  {14,big,<<X:32/unsigned-big>>};
enc_rtnetlink_route_metrics({I,Endian,X}) -> {I,Endian,X}.
dec_rtnetlink_addr({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_addr({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_rtnetlink_addr({1,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {address,{X1,X2,X3,X4}};
dec_rtnetlink_addr({1,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {address,{X1,X2,X3,X4}};
dec_rtnetlink_addr({1,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {address,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_addr({1,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {address,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_addr({2,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {local,{X1,X2,X3,X4}};
dec_rtnetlink_addr({2,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {local,{X1,X2,X3,X4}};
dec_rtnetlink_addr({2,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {local,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_addr({2,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {local,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_addr({3,native,<<X/binary>>}) ->
  {label,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_rtnetlink_addr({3,big,<<X/binary>>}) ->
  {label,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_rtnetlink_addr({4,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {broadcast,{X1,X2,X3,X4}};
dec_rtnetlink_addr({4,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {broadcast,{X1,X2,X3,X4}};
dec_rtnetlink_addr({4,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {broadcast,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_addr({4,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {broadcast,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_addr({5,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {anycast,{X1,X2,X3,X4}};
dec_rtnetlink_addr({5,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {anycast,{X1,X2,X3,X4}};
dec_rtnetlink_addr({5,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {anycast,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_addr({5,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {anycast,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_addr({6,native,<<X/binary>>}) ->
  {cacheinfo,[Xi || <<Xi:32/unsigned-native>> <= X]};
dec_rtnetlink_addr({6,big,<<X/binary>>}) ->
  {cacheinfo,[Xi || <<Xi:32/unsigned-big>> <= X]};
dec_rtnetlink_addr({7,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {multicast,{X1,X2,X3,X4}};
dec_rtnetlink_addr({7,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {multicast,{X1,X2,X3,X4}};
dec_rtnetlink_addr({7,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {multicast,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_addr({7,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {multicast,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_rtnetlink_addr({I,_Endian,Bin}) -> {I,Bin}.
enc_rtnetlink_addr({unspec,native,X}) ->
  {0,native,<<X>>};
enc_rtnetlink_addr({unspec,big,X}) ->
  {0,big,<<X>>};
enc_rtnetlink_addr({address,native,{X1,X2,X3,X4}}) ->
  {1,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_addr({address,big,{X1,X2,X3,X4}}) ->
  {1,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_addr({address,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {1,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_addr({address,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {1,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_addr({local,native,{X1,X2,X3,X4}}) ->
  {2,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_addr({local,big,{X1,X2,X3,X4}}) ->
  {2,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_addr({local,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {2,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_addr({local,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {2,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_addr({label,native,X}) ->
  {3,native,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_rtnetlink_addr({label,big,X}) ->
  {3,big,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_rtnetlink_addr({broadcast,native,{X1,X2,X3,X4}}) ->
  {4,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_addr({broadcast,big,{X1,X2,X3,X4}}) ->
  {4,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_addr({broadcast,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {4,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_addr({broadcast,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {4,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_addr({anycast,native,{X1,X2,X3,X4}}) ->
  {5,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_addr({anycast,big,{X1,X2,X3,X4}}) ->
  {5,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_addr({anycast,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {5,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_addr({anycast,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {5,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_addr({cacheinfo,native,X}) ->
  {6,native,<<(<< <<Xi:32/unsigned-native>> || Xi <- X>>)/binary>>};
enc_rtnetlink_addr({cacheinfo,big,X}) ->
  {6,big,<<(<< <<Xi:32/unsigned-big>> || Xi <- X>>)/binary>>};
enc_rtnetlink_addr({multicast,native,{X1,X2,X3,X4}}) ->
  {7,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_addr({multicast,big,{X1,X2,X3,X4}}) ->
  {7,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_rtnetlink_addr({multicast,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {7,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_addr({multicast,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {7,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_rtnetlink_addr({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_tuple_ip({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_tuple_ip({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_tuple_ip({1,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {v4_src,{X1,X2,X3,X4}};
dec_ctnetlink_tuple_ip({1,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {v4_src,{X1,X2,X3,X4}};
dec_ctnetlink_tuple_ip({2,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {v4_dst,{X1,X2,X3,X4}};
dec_ctnetlink_tuple_ip({2,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {v4_dst,{X1,X2,X3,X4}};
dec_ctnetlink_tuple_ip({3,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {v6_src,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_ctnetlink_tuple_ip({3,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {v6_src,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_ctnetlink_tuple_ip({4,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {v6_dst,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_ctnetlink_tuple_ip({4,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {v6_dst,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_ctnetlink_tuple_ip({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_tuple_ip({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_tuple_ip({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_tuple_ip({v4_src,native,{X1,X2,X3,X4}}) ->
  {1,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_ctnetlink_tuple_ip({v4_src,big,{X1,X2,X3,X4}}) ->
  {1,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_ctnetlink_tuple_ip({v4_dst,native,{X1,X2,X3,X4}}) ->
  {2,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_ctnetlink_tuple_ip({v4_dst,big,{X1,X2,X3,X4}}) ->
  {2,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_ctnetlink_tuple_ip({v6_src,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {3,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_ctnetlink_tuple_ip({v6_src,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {3,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_ctnetlink_tuple_ip({v6_dst,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {4,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_ctnetlink_tuple_ip({v6_dst,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {4,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_ctnetlink_tuple_ip({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_protoinfo_tcp({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_protoinfo_tcp({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_protoinfo_tcp({1,native,<<Xe:8/unsigned-native>>}) ->
  {state,dec_ctnetlink_protoinfo_tcp_state(Xe)};
dec_ctnetlink_protoinfo_tcp({1,big,<<Xe:8/unsigned-big>>}) ->
  {state,dec_ctnetlink_protoinfo_tcp_state(Xe)};
dec_ctnetlink_protoinfo_tcp({2,native,<<X:8/unsigned-native>>}) ->
  {wscale_original,X};
dec_ctnetlink_protoinfo_tcp({2,big,<<X:8/unsigned-big>>}) ->
  {wscale_original,X};
dec_ctnetlink_protoinfo_tcp({3,native,<<X:8/unsigned-native>>}) ->
  {wscale_reply,X};
dec_ctnetlink_protoinfo_tcp({3,big,<<X:8/unsigned-big>>}) ->
  {wscale_reply,X};
dec_ctnetlink_protoinfo_tcp({4,native,<<X:16/unsigned-native>>}) ->
  {flags_original,X};
dec_ctnetlink_protoinfo_tcp({4,big,<<X:16/unsigned-big>>}) ->
  {flags_original,X};
dec_ctnetlink_protoinfo_tcp({5,native,<<X:16/unsigned-native>>}) ->
  {flags_reply,X};
dec_ctnetlink_protoinfo_tcp({5,big,<<X:16/unsigned-big>>}) ->
  {flags_reply,X};
dec_ctnetlink_protoinfo_tcp({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_protoinfo_tcp({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_protoinfo_tcp({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_protoinfo_tcp({state,native,X}) ->
  {1,native,<<(enc_ctnetlink_protoinfo_tcp_state(X)):8/unsigned-native>>};
enc_ctnetlink_protoinfo_tcp({state,big,X}) ->
  {1,big,<<(enc_ctnetlink_protoinfo_tcp_state(X)):8/unsigned-big>>};
enc_ctnetlink_protoinfo_tcp({wscale_original,native,X}) ->
  {2,native,<<X:8/unsigned-native>>};
enc_ctnetlink_protoinfo_tcp({wscale_original,big,X}) ->
  {2,big,<<X:8/unsigned-big>>};
enc_ctnetlink_protoinfo_tcp({wscale_reply,native,X}) ->
  {3,native,<<X:8/unsigned-native>>};
enc_ctnetlink_protoinfo_tcp({wscale_reply,big,X}) ->
  {3,big,<<X:8/unsigned-big>>};
enc_ctnetlink_protoinfo_tcp({flags_original,native,X}) ->
  {4,native,<<X:16/unsigned-native>>};
enc_ctnetlink_protoinfo_tcp({flags_original,big,X}) ->
  {4,big,<<X:16/unsigned-big>>};
enc_ctnetlink_protoinfo_tcp({flags_reply,native,X}) ->
  {5,native,<<X:16/unsigned-native>>};
enc_ctnetlink_protoinfo_tcp({flags_reply,big,X}) ->
  {5,big,<<X:16/unsigned-big>>};
enc_ctnetlink_protoinfo_tcp({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_help({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_help({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_help({1,native,<<X/binary>>}) ->
  {name,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_ctnetlink_help({1,big,<<X/binary>>}) ->
  {name,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_ctnetlink_help({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_help({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_help({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_help({name,native,X}) ->
  {1,native,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_ctnetlink_help({name,big,X}) ->
  {1,big,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_ctnetlink_help({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_timestamp({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_timestamp({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_timestamp({1,native,<<X:64/unsigned-native>>}) ->
  {start,X};
dec_ctnetlink_timestamp({1,big,<<X:64/unsigned-big>>}) ->
  {start,X};
dec_ctnetlink_timestamp({2,native,<<X:64/unsigned-native>>}) ->
  {stop,X};
dec_ctnetlink_timestamp({2,big,<<X:64/unsigned-big>>}) ->
  {stop,X};
dec_ctnetlink_timestamp({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_timestamp({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_timestamp({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_timestamp({start,native,X}) ->
  {1,native,<<X:64/unsigned-native>>};
enc_ctnetlink_timestamp({start,big,X}) ->
  {1,big,<<X:64/unsigned-big>>};
enc_ctnetlink_timestamp({stop,native,X}) ->
  {2,native,<<X:64/unsigned-native>>};
enc_ctnetlink_timestamp({stop,big,X}) ->
  {2,big,<<X:64/unsigned-big>>};
enc_ctnetlink_timestamp({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_exp_tuple_ip({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_exp_tuple_ip({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_exp_tuple_ip({1,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {v4_src,{X1,X2,X3,X4}};
dec_ctnetlink_exp_tuple_ip({1,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {v4_src,{X1,X2,X3,X4}};
dec_ctnetlink_exp_tuple_ip({2,native,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {v4_dst,{X1,X2,X3,X4}};
dec_ctnetlink_exp_tuple_ip({2,big,<<X1:8,X2:8,X3:8,X4:8>>}) ->
  {v4_dst,{X1,X2,X3,X4}};
dec_ctnetlink_exp_tuple_ip({3,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {v6_src,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_ctnetlink_exp_tuple_ip({3,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {v6_src,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_ctnetlink_exp_tuple_ip({4,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {v6_dst,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_ctnetlink_exp_tuple_ip({4,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>}) ->
  {v6_dst,{X1,X2,X3,X4,X5,X6,X7,X8}};
dec_ctnetlink_exp_tuple_ip({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_exp_tuple_ip({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_exp_tuple_ip({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_exp_tuple_ip({v4_src,native,{X1,X2,X3,X4}}) ->
  {1,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_ctnetlink_exp_tuple_ip({v4_src,big,{X1,X2,X3,X4}}) ->
  {1,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_ctnetlink_exp_tuple_ip({v4_dst,native,{X1,X2,X3,X4}}) ->
  {2,native,<<X1:8,X2:8,X3:8,X4:8>>};
enc_ctnetlink_exp_tuple_ip({v4_dst,big,{X1,X2,X3,X4}}) ->
  {2,big,<<X1:8,X2:8,X3:8,X4:8>>};
enc_ctnetlink_exp_tuple_ip({v6_src,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {3,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_ctnetlink_exp_tuple_ip({v6_src,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {3,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_ctnetlink_exp_tuple_ip({v6_dst,native,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {4,native,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_ctnetlink_exp_tuple_ip({v6_dst,big,{X1,X2,X3,X4,X5,X6,X7,X8}}) ->
  {4,big,<<X1:16,X2:16,X3:16,X4:16,X5:16,X6:16,X7:16,X8:16>>};
enc_ctnetlink_exp_tuple_ip({I,Endian,X}) -> {I,Endian,X}.
dec_ctnetlink_exp({0,native,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_exp({0,big,<<X/binary>>}) ->
  {unspec,X};
dec_ctnetlink_exp({1,native,<<X/binary>>}) ->
  {master,dec_ctnetlink_exp_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink_exp({1,big,<<X/binary>>}) ->
  {master,dec_ctnetlink_exp_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink_exp({2,native,<<X/binary>>}) ->
  {tuple,dec_ctnetlink_exp_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink_exp({2,big,<<X/binary>>}) ->
  {tuple,dec_ctnetlink_exp_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink_exp({3,native,<<X/binary>>}) ->
  {mask,dec_ctnetlink_exp_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink_exp({3,big,<<X/binary>>}) ->
  {mask,dec_ctnetlink_exp_tuple(netlink_codec:decode_tlv(X))};
dec_ctnetlink_exp({4,native,<<X:32/unsigned-native>>}) ->
  {timeout,X};
dec_ctnetlink_exp({4,big,<<X:32/unsigned-big>>}) ->
  {timeout,X};
dec_ctnetlink_exp({5,native,<<X:32/unsigned-native>>}) ->
  {id,X};
dec_ctnetlink_exp({5,big,<<X:32/unsigned-big>>}) ->
  {id,X};
dec_ctnetlink_exp({6,native,<<X/binary>>}) ->
  {help_name,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_ctnetlink_exp({6,big,<<X/binary>>}) ->
  {help_name,binary_to_list(hd(binary:split(X,<<0>>)))};
dec_ctnetlink_exp({7,native,<<X:16/unsigned-native>>}) ->
  {zone,X};
dec_ctnetlink_exp({7,big,<<X:16/unsigned-big>>}) ->
  {zone,X};
dec_ctnetlink_exp({8,native,<<Xf:32/unsigned-native>>}) ->
  {flags,netlink_codec:decode_flags(Xf,fun dec_ctnetlink_exp_flags/1)};
dec_ctnetlink_exp({8,big,<<Xf:32/unsigned-big>>}) ->
  {flags,netlink_codec:decode_flags(Xf,fun dec_ctnetlink_exp_flags/1)};
dec_ctnetlink_exp({I,_Endian,Bin}) -> {I,Bin}.
enc_ctnetlink_exp({unspec,native,X}) ->
  {0,native,<<X>>};
enc_ctnetlink_exp({unspec,big,X}) ->
  {0,big,<<X>>};
enc_ctnetlink_exp({master,native,X}) ->
  {1,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_exp_tuple(X)))>>};
enc_ctnetlink_exp({master,big,X}) ->
  {1,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_exp_tuple(X)))>>};
enc_ctnetlink_exp({tuple,native,X}) ->
  {2,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_exp_tuple(X)))>>};
enc_ctnetlink_exp({tuple,big,X}) ->
  {2,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_exp_tuple(X)))>>};
enc_ctnetlink_exp({mask,native,X}) ->
  {3,native,<<(netlink_codec:encode_tlv(enc_ctnetlink_exp_tuple(X)))>>};
enc_ctnetlink_exp({mask,big,X}) ->
  {3,big,<<(netlink_codec:encode_tlv(enc_ctnetlink_exp_tuple(X)))>>};
enc_ctnetlink_exp({timeout,native,X}) ->
  {4,native,<<X:32/unsigned-native>>};
enc_ctnetlink_exp({timeout,big,X}) ->
  {4,big,<<X:32/unsigned-big>>};
enc_ctnetlink_exp({id,native,X}) ->
  {5,native,<<X:32/unsigned-native>>};
enc_ctnetlink_exp({id,big,X}) ->
  {5,big,<<X:32/unsigned-big>>};
enc_ctnetlink_exp({help_name,native,X}) ->
  {6,native,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_ctnetlink_exp({help_name,big,X}) ->
  {6,big,<<(erlang:iolist_to_binary([X,0]))/binary>>};
enc_ctnetlink_exp({zone,native,X}) ->
  {7,native,<<X:16/unsigned-native>>};
enc_ctnetlink_exp({zone,big,X}) ->
  {7,big,<<X:16/unsigned-big>>};
enc_ctnetlink_exp({flags,native,X}) ->
  {8,native,<<(netlink_codec:encode_flags(X,fun enc_ctnetlink_exp_flags/1)):32/unsigned-native>>};
enc_ctnetlink_exp({flags,big,X}) ->
  {8,big,<<(netlink_codec:encode_flags(X,fun enc_ctnetlink_exp_flags/1)):32/unsigned-big>>};
enc_ctnetlink_exp({I,Endian,X}) -> {I,Endian,X}.
dec_overrun({native,<<X1:32/unsigned-native>>}) ->
   #overrun{status=X1}.
enc_overrun({_Endian,#overrun{status=X1}}) -> <<X1:32/unsigned-native>>.
dec_newlink({native,<<X1e:8/unsigned-native,_:8/unsigned-native,X3e:16/unsigned-native,X4:32/signed-native,X5f:32/unsigned-native,X6f:32/unsigned-native,X7/binary>>}) ->
   #newlink{family=dec_family(X1e),arphrd=dec_arphrd(X3e),index=X4,flags=netlink_codec:decode_flags(X5f,fun dec_iff_flags/1),change=netlink_codec:decode_flags(X6f,fun dec_iff_flags/1),attributes=[dec_rtnetlink_link(X7i) || X7i <- netlink_codec:decode_tlv_list(X7)]}.
enc_newlink({_Endian,#newlink{family=X1,arphrd=X3,index=X4,flags=X5,change=X6,attributes=X7}}) -> <<(enc_family(X1)):8/unsigned-native,0:8/unsigned-native,(enc_arphrd(X3)):16/unsigned-native,X4:32/signed-native,(netlink_codec:encode_flags(X5,fun enc_iff_flags/1)):32/unsigned-native,(netlink_codec:encode_flags(X6,fun enc_iff_flags/1)):32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_link(X7i) || X7i <- X7]))/binary>>.
dec_dellink({native,<<X1e:8/unsigned-native,_:8/unsigned-native,X3e:16/unsigned-native,X4:32/signed-native,X5f:32/unsigned-native,X6f:32/unsigned-native,X7/binary>>}) ->
   #dellink{family=dec_family(X1e),arphrd=dec_arphrd(X3e),index=X4,flags=netlink_codec:decode_flags(X5f,fun dec_iff_flags/1),change=netlink_codec:decode_flags(X6f,fun dec_iff_flags/1),attributes=[dec_rtnetlink_link(X7i) || X7i <- netlink_codec:decode_tlv_list(X7)]}.
enc_dellink({_Endian,#dellink{family=X1,arphrd=X3,index=X4,flags=X5,change=X6,attributes=X7}}) -> <<(enc_family(X1)):8/unsigned-native,0:8/unsigned-native,(enc_arphrd(X3)):16/unsigned-native,X4:32/signed-native,(netlink_codec:encode_flags(X5,fun enc_iff_flags/1)):32/unsigned-native,(netlink_codec:encode_flags(X6,fun enc_iff_flags/1)):32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_link(X7i) || X7i <- X7]))/binary>>.
dec_getlink({native,<<X1e:8/unsigned-native,_:8/unsigned-native,X3e:16/unsigned-native,X4:32/signed-native,X5f:32/unsigned-native,X6f:32/unsigned-native,X7/binary>>}) ->
   #getlink{family=dec_family(X1e),arphrd=dec_arphrd(X3e),index=X4,flags=netlink_codec:decode_flags(X5f,fun dec_iff_flags/1),change=netlink_codec:decode_flags(X6f,fun dec_iff_flags/1),attributes=[dec_rtnetlink_link(X7i) || X7i <- netlink_codec:decode_tlv_list(X7)]}.
enc_getlink({_Endian,#getlink{family=X1,arphrd=X3,index=X4,flags=X5,change=X6,attributes=X7}}) -> <<(enc_family(X1)):8/unsigned-native,0:8/unsigned-native,(enc_arphrd(X3)):16/unsigned-native,X4:32/signed-native,(netlink_codec:encode_flags(X5,fun enc_iff_flags/1)):32/unsigned-native,(netlink_codec:encode_flags(X6,fun enc_iff_flags/1)):32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_link(X7i) || X7i <- X7]))/binary>>.
dec_newneigh({native,<<X1e:8/unsigned-native,_:8/unsigned-native,_:16/unsigned-native,X4:32/unsigned-native,X5:16/unsigned-native,X6:8/unsigned-native,X7:8/unsigned-native,X8/binary>>}) ->
   #newneigh{family=dec_family(X1e),index=X4,state=X5,flags=X6,nmd_type=X7,attributes=[dec_rtnetlink_neigh(X8i) || X8i <- netlink_codec:decode_tlv_list(X8)]}.
enc_newneigh({_Endian,#newneigh{family=X1,index=X4,state=X5,flags=X6,nmd_type=X7,attributes=X8}}) -> <<(enc_family(X1)):8/unsigned-native,0:8/unsigned-native,0:16/unsigned-native,X4:32/unsigned-native,X5:16/unsigned-native,X6:8/unsigned-native,X7:8/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_neigh(X8i) || X8i <- X8]))/binary>>.
dec_delneigh({native,<<X1e:8/unsigned-native,_:8/unsigned-native,_:16/unsigned-native,X4:32/unsigned-native,X5:16/unsigned-native,X6:8/unsigned-native,X7:8/unsigned-native,X8/binary>>}) ->
   #delneigh{family=dec_family(X1e),index=X4,state=X5,flags=X6,nmd_type=X7,attributes=[dec_rtnetlink_neigh(X8i) || X8i <- netlink_codec:decode_tlv_list(X8)]}.
enc_delneigh({_Endian,#delneigh{family=X1,index=X4,state=X5,flags=X6,nmd_type=X7,attributes=X8}}) -> <<(enc_family(X1)):8/unsigned-native,0:8/unsigned-native,0:16/unsigned-native,X4:32/unsigned-native,X5:16/unsigned-native,X6:8/unsigned-native,X7:8/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_neigh(X8i) || X8i <- X8]))/binary>>.
dec_getneigh({native,<<X1e:8/unsigned-native,_:8/unsigned-native,_:16/unsigned-native,X4:32/unsigned-native,X5:16/unsigned-native,X6:8/unsigned-native,X7:8/unsigned-native,X8/binary>>}) ->
   #getneigh{family=dec_family(X1e),index=X4,state=X5,flags=X6,nmd_type=X7,attributes=[dec_rtnetlink_neigh(X8i) || X8i <- netlink_codec:decode_tlv_list(X8)]}.
enc_getneigh({_Endian,#getneigh{family=X1,index=X4,state=X5,flags=X6,nmd_type=X7,attributes=X8}}) -> <<(enc_family(X1)):8/unsigned-native,0:8/unsigned-native,0:16/unsigned-native,X4:32/unsigned-native,X5:16/unsigned-native,X6:8/unsigned-native,X7:8/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_neigh(X8i) || X8i <- X8]))/binary>>.
dec_ifaddrmsg({native,<<X1e:8/unsigned-native,X2:8/unsigned-native,X3f:8/unsigned-native,X4:8/unsigned-native,X5:32/unsigned-native,X6/binary>>}) ->
   #ifaddrmsg{family=dec_family(X1e),prefixlen=X2,flags=netlink_codec:decode_flags(X3f,fun dec_ifa_flags/1),scope=X4,index=X5,attributes=[dec_rtnetlink_addr(X6i) || X6i <- netlink_codec:decode_tlv_list(X6)]}.
enc_ifaddrmsg({_Endian,#ifaddrmsg{family=X1,prefixlen=X2,flags=X3,scope=X4,index=X5,attributes=X6}}) -> <<(enc_family(X1)):8/unsigned-native,X2:8/unsigned-native,(netlink_codec:encode_flags(X3,fun enc_ifa_flags/1)):8/unsigned-native,X4:8/unsigned-native,X5:32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_addr(X6i) || X6i <- X6]))/binary>>.
dec_ifinfomsg({native,<<X1e:8/unsigned-native,_:8/unsigned-native,X3e:16/unsigned-native,X4:32/signed-native,X5f:32/unsigned-native,X6f:32/unsigned-native,X7/binary>>}) ->
   #ifinfomsg{family=dec_family(X1e),arphrd=dec_arphrd(X3e),index=X4,flags=netlink_codec:decode_flags(X5f,fun dec_iff_flags/1),change=netlink_codec:decode_flags(X6f,fun dec_iff_flags/1),attributes=[dec_rtnetlink_link(X7i) || X7i <- netlink_codec:decode_tlv_list(X7)]}.
enc_ifinfomsg({_Endian,#ifinfomsg{family=X1,arphrd=X3,index=X4,flags=X5,change=X6,attributes=X7}}) -> <<(enc_family(X1)):8/unsigned-native,0:8/unsigned-native,(enc_arphrd(X3)):16/unsigned-native,X4:32/signed-native,(netlink_codec:encode_flags(X5,fun enc_iff_flags/1)):32/unsigned-native,(netlink_codec:encode_flags(X6,fun enc_iff_flags/1)):32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_link(X7i) || X7i <- X7]))/binary>>.
dec_rtmsg({native,<<X1e:8/unsigned-native,X2:8/unsigned-native,X3:8/unsigned-native,X4:8/unsigned-native,X5:8/unsigned-native,X6e:8/unsigned-native,X7:8/unsigned-native,X8:8/unsigned-native,X9:32/unsigned-native,X10/binary>>}) ->
   #rtmsg{family=dec_family(X1e),dstlen=X2,srclen=X3,tos=X4,table=X5,protocol=dec_protocol(X6e),scope=X7,rtm_type=X8,flags=X9,attributes=[dec_rtnetlink_route(X10i) || X10i <- netlink_codec:decode_tlv_list(X10)]}.
enc_rtmsg({_Endian,#rtmsg{family=X1,dstlen=X2,srclen=X3,tos=X4,table=X5,protocol=X6,scope=X7,rtm_type=X8,flags=X9,attributes=X10}}) -> <<(enc_family(X1)):8/unsigned-native,X2:8/unsigned-native,X3:8/unsigned-native,X4:8/unsigned-native,X5:8/unsigned-native,(enc_protocol(X6)):8/unsigned-native,X7:8/unsigned-native,X8:8/unsigned-native,X9:32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_route(X10i) || X10i <- X10]))/binary>>.
dec_ndmsg({native,<<X1e:8/unsigned-native,_:8/unsigned-native,_:16/unsigned-native,X4:32/unsigned-native,X5:16/unsigned-native,X6:8/unsigned-native,X7:8/unsigned-native,X8/binary>>}) ->
   #ndmsg{family=dec_family(X1e),index=X4,state=X5,flags=X6,nmd_type=X7,attributes=[dec_rtnetlink_neigh(X8i) || X8i <- netlink_codec:decode_tlv_list(X8)]}.
enc_ndmsg({_Endian,#ndmsg{family=X1,index=X4,state=X5,flags=X6,nmd_type=X7,attributes=X8}}) -> <<(enc_family(X1)):8/unsigned-native,0:8/unsigned-native,0:16/unsigned-native,X4:32/unsigned-native,X5:16/unsigned-native,X6:8/unsigned-native,X7:8/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_neigh(X8i) || X8i <- X8]))/binary>>.
dec_newroute({native,<<X1e:8/unsigned-native,X2:8/unsigned-native,X3:8/unsigned-native,X4:8/unsigned-native,X5:8/unsigned-native,X6e:8/unsigned-native,X7:8/unsigned-native,X8:8/unsigned-native,X9:32/unsigned-native,X10/binary>>}) ->
   #newroute{family=dec_family(X1e),dstlen=X2,srclen=X3,tos=X4,table=X5,protocol=dec_protocol(X6e),scope=X7,rtm_type=X8,flags=X9,attributes=[dec_rtnetlink_route(X10i) || X10i <- netlink_codec:decode_tlv_list(X10)]}.
enc_newroute({_Endian,#newroute{family=X1,dstlen=X2,srclen=X3,tos=X4,table=X5,protocol=X6,scope=X7,rtm_type=X8,flags=X9,attributes=X10}}) -> <<(enc_family(X1)):8/unsigned-native,X2:8/unsigned-native,X3:8/unsigned-native,X4:8/unsigned-native,X5:8/unsigned-native,(enc_protocol(X6)):8/unsigned-native,X7:8/unsigned-native,X8:8/unsigned-native,X9:32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_route(X10i) || X10i <- X10]))/binary>>.
dec_delroute({native,<<X1e:8/unsigned-native,X2:8/unsigned-native,X3:8/unsigned-native,X4:8/unsigned-native,X5:8/unsigned-native,X6e:8/unsigned-native,X7:8/unsigned-native,X8:8/unsigned-native,X9:32/unsigned-native,X10/binary>>}) ->
   #delroute{family=dec_family(X1e),dstlen=X2,srclen=X3,tos=X4,table=X5,protocol=dec_protocol(X6e),scope=X7,rtm_type=X8,flags=X9,attributes=[dec_rtnetlink_route(X10i) || X10i <- netlink_codec:decode_tlv_list(X10)]}.
enc_delroute({_Endian,#delroute{family=X1,dstlen=X2,srclen=X3,tos=X4,table=X5,protocol=X6,scope=X7,rtm_type=X8,flags=X9,attributes=X10}}) -> <<(enc_family(X1)):8/unsigned-native,X2:8/unsigned-native,X3:8/unsigned-native,X4:8/unsigned-native,X5:8/unsigned-native,(enc_protocol(X6)):8/unsigned-native,X7:8/unsigned-native,X8:8/unsigned-native,X9:32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_route(X10i) || X10i <- X10]))/binary>>.
dec_getroute({native,<<X1e:8/unsigned-native,X2:8/unsigned-native,X3:8/unsigned-native,X4:8/unsigned-native,X5:8/unsigned-native,X6e:8/unsigned-native,X7:8/unsigned-native,X8:8/unsigned-native,X9:32/unsigned-native,X10/binary>>}) ->
   #getroute{family=dec_family(X1e),dstlen=X2,srclen=X3,tos=X4,table=X5,protocol=dec_protocol(X6e),scope=X7,rtm_type=X8,flags=X9,attributes=[dec_rtnetlink_route(X10i) || X10i <- netlink_codec:decode_tlv_list(X10)]}.
enc_getroute({_Endian,#getroute{family=X1,dstlen=X2,srclen=X3,tos=X4,table=X5,protocol=X6,scope=X7,rtm_type=X8,flags=X9,attributes=X10}}) -> <<(enc_family(X1)):8/unsigned-native,X2:8/unsigned-native,X3:8/unsigned-native,X4:8/unsigned-native,X5:8/unsigned-native,(enc_protocol(X6)):8/unsigned-native,X7:8/unsigned-native,X8:8/unsigned-native,X9:32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_route(X10i) || X10i <- X10]))/binary>>.
dec_done({native,<<X1:32/unsigned-native>>}) ->
   #done{status=X1}.
enc_done({_Endian,#done{status=X1}}) -> <<X1:32/unsigned-native>>.
dec_nlmsghdr({native,<<X1:32/unsigned-native,X2:16/unsigned-native,X3:16/unsigned-native,X4:32/unsigned-native,X5:32/unsigned-native>>}) ->
   #nlmsghdr{len=X1,type=X2,flags=X3,seq=X4,pid=X5}.
enc_nlmsghdr({_Endian,#nlmsghdr{len=X1,type=X2,flags=X3,seq=X4,pid=X5}}) -> <<X1:32/unsigned-native,X2:16/unsigned-native,X3:16/unsigned-native,X4:32/unsigned-native,X5:32/unsigned-native>>.
dec_newaddr({native,<<X1e:8/unsigned-native,X2:8/unsigned-native,X3f:8/unsigned-native,X4:8/unsigned-native,X5:32/unsigned-native,X6/binary>>}) ->
   #newaddr{family=dec_family(X1e),prefixlen=X2,flags=netlink_codec:decode_flags(X3f,fun dec_ifa_flags/1),scope=X4,index=X5,attributes=[dec_rtnetlink_addr(X6i) || X6i <- netlink_codec:decode_tlv_list(X6)]}.
enc_newaddr({_Endian,#newaddr{family=X1,prefixlen=X2,flags=X3,scope=X4,index=X5,attributes=X6}}) -> <<(enc_family(X1)):8/unsigned-native,X2:8/unsigned-native,(netlink_codec:encode_flags(X3,fun enc_ifa_flags/1)):8/unsigned-native,X4:8/unsigned-native,X5:32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_addr(X6i) || X6i <- X6]))/binary>>.
dec_deladdr({native,<<X1e:8/unsigned-native,X2:8/unsigned-native,X3f:8/unsigned-native,X4:8/unsigned-native,X5:32/unsigned-native,X6/binary>>}) ->
   #deladdr{family=dec_family(X1e),prefixlen=X2,flags=netlink_codec:decode_flags(X3f,fun dec_ifa_flags/1),scope=X4,index=X5,attributes=[dec_rtnetlink_addr(X6i) || X6i <- netlink_codec:decode_tlv_list(X6)]}.
enc_deladdr({_Endian,#deladdr{family=X1,prefixlen=X2,flags=X3,scope=X4,index=X5,attributes=X6}}) -> <<(enc_family(X1)):8/unsigned-native,X2:8/unsigned-native,(netlink_codec:encode_flags(X3,fun enc_ifa_flags/1)):8/unsigned-native,X4:8/unsigned-native,X5:32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_addr(X6i) || X6i <- X6]))/binary>>.
dec_getaddr({native,<<X1e:8/unsigned-native,X2:8/unsigned-native,X3f:8/unsigned-native,X4:8/unsigned-native,X5:32/unsigned-native,X6/binary>>}) ->
   #getaddr{family=dec_family(X1e),prefixlen=X2,flags=netlink_codec:decode_flags(X3f,fun dec_ifa_flags/1),scope=X4,index=X5,attributes=[dec_rtnetlink_addr(X6i) || X6i <- netlink_codec:decode_tlv_list(X6)]}.
enc_getaddr({_Endian,#getaddr{family=X1,prefixlen=X2,flags=X3,scope=X4,index=X5,attributes=X6}}) -> <<(enc_family(X1)):8/unsigned-native,X2:8/unsigned-native,(netlink_codec:encode_flags(X3,fun enc_ifa_flags/1)):8/unsigned-native,X4:8/unsigned-native,X5:32/unsigned-native,(netlink_codec:encode_tlv_list([enc_rtnetlink_addr(X6i) || X6i <- X6]))/binary>>.
dec_error({native,<<X1:32/signed-native,X21:32/unsigned-native,X22:16/unsigned-native,X23:16/unsigned-native,X24:32/unsigned-native,X25:32/unsigned-native,X3/binary>>}) ->
   #error{errno=X1,msg=#nlmsghdr{len=X21,type=X22,flags=X23,seq=X24,pid=X25},data=X3}.
enc_error({_Endian,#error{errno=X1,msg=#nlmsghdr{len=X21,type=X22,flags=X23,seq=X24,pid=X25},data=X3}}) -> <<X1:32/signed-native,X21:32/unsigned-native,X22:16/unsigned-native,X23:16/unsigned-native,X24:32/unsigned-native,X25:32/unsigned-native,X3>>.
dec_if_map({native,<<X1:64/unsigned-native,X2:64/unsigned-native,X3:64/unsigned-native,X4:16/unsigned-native,X5:8/unsigned-native,X6:8/unsigned-native>>}) ->
   #if_map{memstart=X1,memend=X2,baseaddr=X3,irq=X4,dma=X5,port=X6}.
enc_if_map({_Endian,#if_map{memstart=X1,memend=X2,baseaddr=X3,irq=X4,dma=X5,port=X6}}) -> <<X1:64/unsigned-native,X2:64/unsigned-native,X3:64/unsigned-native,X4:16/unsigned-native,X5:8/unsigned-native,X6:8/unsigned-native>>.
dec_noop({native,<<>>}) ->
   #noop{}.
enc_noop({_Endian,#noop{}}) -> <<>>.
