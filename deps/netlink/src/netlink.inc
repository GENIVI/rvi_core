%% -*- erlang -*-
%%   
%%  nlink data definitions
%% 
{define, 'NLMSG_NOOP', 1}.
{define, 'NLMSG_ERROR', 2}.
{define, 'NLMSG_DONE', 3}.
{define, 'NLMSG_OVERRUN', 4}.

{define, 'RTM_NEWLINK', 16}.
{define, 'RTM_DELLINK', 17}.
{define, 'RTM_GETLINK', 18}.
{define, 'RTM_SETLINK', 19}.
{define, 'RTM_NEWADDR', 20}.
{define, 'RTM_DELADDR', 21}.
{define, 'RTM_GETADDR', 22}.
{define, 'RTM_NEWROUTE', 24}.
{define, 'RTM_DELROUTE', 25}.
{define, 'RTM_GETROUTE', 26}.
{define, 'RTM_NEWNEIGH', 28}.
{define, 'RTM_DELNEIGH', 29}.
{define, 'RTM_GETNEIGH', 30}.
{define, 'RTM_NEWRULE', 32}.
{define, 'RTM_DELRULE', 33}.
{define, 'RTM_GETRULE', 34}.
{define, 'RTM_NEWQDISC', 36}.
{define, 'RTM_DELQDISC', 37}.
{define, 'RTM_GETQDISC', 38}.
{define, 'RTM_NEWTCLASS', 40}.
{define, 'RTM_DELTCLASS', 41}.
{define, 'RTM_GETTCLASS', 42}.
{define, 'RTM_NEWTFILTER', 44}.
{define, 'RTM_DELTFILTER', 45}.
{define, 'RTM_GETTFILTER', 46}.
{define, 'RTM_NEWACTION', 48}.
{define, 'RTM_DELACTION', 49}.
{define, 'RTM_GETACTION', 50}.
{define, 'RTM_NEWPREFIX', 52}.
{define, 'RTM_GETMULTICAST', 58}.
{define, 'RTM_GETANYCAST', 62}.
{define, 'RTM_NEWNEIGHTBL', 64}.
{define, 'RTM_GETNEIGHTBL', 66}.
{define, 'RTM_SETNEIGHTBL', 67}.
{define, 'RTM_NEWNDUSEROPT', 68}.
{define, 'RTM_NEWADDRLABEL', 72}.
{define, 'RTM_DELADDRLABEL', 73}.
{define, 'RTM_GETADDRLABEL', 74}.
{define, 'RTM_GETDCB', 78}.
{define, 'RTM_SETDCB', 79}.

{define, 'AF_UNSPEC', 0}.
{define, 'AF_LOCAL',  1}.
{define, 'AF_UNIX',   1}.
{define, 'AF_FILE',   1}.
{define, 'AF_INET',   2}.
{define, 'AF_AX25',   3}.
{define, 'AF_IPX',    4}.
{define, 'AF_APPLETALK', 5}.
{define, 'AF_NETROM', 6}.
{define, 'AF_BRIDGE', 7}.
{define, 'AF_ATMPVC', 8}.
{define, 'AF_X25', 9}.
{define, 'AF_INET6', 10}.
{define, 'AF_ROSE', 11}.
{define, 'AF_DECNET', 12}.
{define, 'AF_NETBEUI', 13}.
{define, 'AF_SECURITY', 14}.
{define, 'AF_KEY', 15}.
{define, 'AF_NETLINK', 16}.
{define, 'AF_ROUTE', 16}.
{define, 'AF_PACKET', 17}.
{define, 'AF_ASH', 18}.
{define, 'AF_ECONET', 19}.
{define, 'AF_ATMSVC', 20}.
{define, 'AF_RDS', 21}.
{define, 'AF_SNA', 22}.
{define, 'AF_IRDA', 23}.
{define, 'AF_PPPOX', 24}.
{define, 'AF_WANPIPE', 25}.
{define, 'AF_LLC', 26}.
{define, 'AF_CAN', 29}.
{define, 'AF_TIPC', 30}.
{define, 'AF_BLUETOOTH', 31}.
{define, 'AF_IUCV', 32}.
{define, 'AF_RXRPC', 33}.
{define, 'AF_ISDN', 34}.
{define, 'AF_PHONET', 35}.
{define, 'AF_IEEE802154', 36}.

{define, 'PROTO_IP', 0}.
{define, 'PROTO_ICMP', 1}.
{define, 'PROTO_IGMP',  2}.
{define, 'PROTO_IPIP',  4}.
{define, 'PROTO_TCP',  6}.
{define, 'PROTO_EGP',  8}.
{define, 'PROTO_PUP',  12}.
{define, 'PROTO_UDP',  17}.
{define, 'PROTO_IDP',  22}.
{define, 'PROTO_TP',  29}.
{define, 'PROTO_DCCP',  33}.
{define, 'PROTO_IPV6',  41}.
{define, 'PROTO_ROUTING',  43}.
{define, 'PROTO_FRAGMENT',  44}.
{define, 'PROTO_RSVP',  46}.
{define, 'PROTO_GRE',  47}.
{define, 'PROTO_ESP',  50}.
{define, 'PROTO_AH',  51}.
{define, 'PROTO_ICMPV6',  58}.
{define, 'PROTO_NONE',  59}.
{define, 'PROTO_DSTOPTS',  60}.
{define, 'PROTO_MTP',  92}.
{define, 'PROTO_ENCAP',  98}.
{define, 'PROTO_PIM',  103}.
{define, 'PROTO_COMP',  108}.
{define, 'PROTO_SCTP',  132}.
{define, 'PROTO_UDPLITE',  136}.
{define, 'PROTO_RAW',  255}.

{define, 'ARPHRD_NETROM', 0}.              %% from KA9Q: NET/ROM pseudo
{define, 'ARPHRD_ETHER ', 1}.               %% Ethernet 10Mbps
{define, 'ARPHRD_EETHER', 2}.              %% Experimental Ethernet
{define, 'ARPHRD_AX25',   3}.              %% AX.25 Level 2
{define, 'ARPHRD_PRONET', 4}.              %% PROnet token ring
{define, 'ARPHRD_CHAOS',  5}.              %% Chaosnet
{define, 'ARPHRD_IEEE802',6}.              %% IEEE 802.2 Ethernet/TR/TB
{define, 'ARPHRD_ARCNET', 7}.              %% ARCnet
{define, 'ARPHRD_APPLETLK', 8}.              %% APPLEtalk
{define, 'ARPHRD_DLCI',     15}.             %% Frame Relay DLCI
{define, 'ARPHRD_ATM',      19}.             %% ATM
{define, 'ARPHRD_METRICOM', 23}.             %% Metricom STRIP (new IANA id)
{define, 'ARPHRD_IEEE1394', 24}.             %% IEEE 1394 IPv4 - RFC 2734
{define, 'ARPHRD_EUI64',    27}.             %% EUI-64
{define, 'ARPHRD_INFINIBAND', 32}.           %% InfiniBand
%% Dummy types for non ARP hardware
{define, 'ARPHRD_SLIP',     256}.
{define, 'ARPHRD_CSLIP',    257}.
{define, 'ARPHRD_SLIP6',    258}.
{define, 'ARPHRD_CSLIP6',   259}.
{define, 'ARPHRD_RSRVD',    260}.           %% Notional KISS type
{define, 'ARPHRD_ADAPT',    264}.
{define, 'ARPHRD_ROSE',     270}.
{define, 'ARPHRD_X25',      271}.            %% CCITT X.25
{define, 'ARPHRD_HWX25',    272}.            %% Boards with X.25 in firmware
{define, 'ARPHRD_CAN',      280}.            %% Controller Area Network
{define, 'ARPHRD_PPP',      512}.
{define, 'ARPHRD_CISCO',    513}.            %% Cisco HDLC
{define, 'ARPHRD_HDLC',     513}.
{define, 'ARPHRD_LAPB',     516}.            %% LAPB
{define, 'ARPHRD_DDCMP',    517}.            %% Digitals DDCMP protocol
{define, 'ARPHRD_RAWHDLC',  518}.            %% Raw HDLC
{define, 'ARPHRD_TUNNEL',   768}.            %% IPIP tunnel
{define, 'ARPHRD_TUNNEL6',  769}.            %% IP6IP6 tunnel
{define, 'ARPHRD_FRAD',     770}.            %% Frame Relay Access Device
{define, 'ARPHRD_SKIP',     771}.            %% SKIP vif
{define, 'ARPHRD_LOOPBACK', 772}.            %% Loopback device
{define, 'ARPHRD_LOCALTLK', 773}.            %% Localtalk device
{define, 'ARPHRD_FDDI',     774}.            %% Fiber Distributed Data Interface
{define, 'ARPHRD_BIF',      775}.            %% AP1000 BIF
{define, 'ARPHRD_SIT',      776}.            %% sit0 device - IPv6-in-IPv4
{define, 'ARPHRD_IPDDP',    777}.            %% IP over DDP tunneller
{define, 'ARPHRD_IPGRE',    778}.            %% GRE over IP
{define, 'ARPHRD_PIMREG',   779}.            %% PIMSM register interface
{define, 'ARPHRD_HIPPI',    780}.            %% High Performance Parallel Interface
{define, 'ARPHRD_ASH',      781}.            %% Nexus 64Mbps Ash
{define, 'ARPHRD_ECONET',   782}.            %% Acorn Econet
{define, 'ARPHRD_IRDA',     783}.            %% Linux-IrDA
%% ARP works differently on different FC media .. so
{define, 'ARPHRD_FCPP',     784}.            %% Point to point fibrechannel
{define, 'ARPHRD_FCAL',     785}.            %% Fibrechannel arbitrated loop
{define, 'ARPHRD_FCPL',     786}.            %% Fibrechannel public loop
{define, 'ARPHRD_FCFABRIC', 787}.            %% Fibrechannel fabric
        %% 787->799 reserved for fibrechannel media types
{define, 'ARPHRD_IEEE802_TR', 800}.          %% Magic type ident for TR
{define, 'ARPHRD_IEEE80211', 801}.           %% IEEE 802.11
{define, 'ARPHRD_IEEE80211_PRISM', 802}.     %% IEEE 802.11 + Prism2 header
{define, 'ARPHRD_IEEE80211_RADIOTAP', 803}.  %% IEEE 802.11 + radiotap header
{define, 'ARPHRD_IEEE802154',  804}.
{define, 'ARPHRD_PHONET',      820}.    %% PhoNet media type
{define, 'ARPHRD_PHONET_PIPE', 821}.    %% PhoNet pipe header
{define, 'ARPHRD_CAIF',     822}.       %% CAIF media type
{define, 'ARPHRD_VOID',   65535}.   %% Void type, nothing is known
{define, 'ARPHRD_NONE',   65534}.   %% zero header length

{enum, family,
 [
  {unspec, 'AF_UNSPEC'},
  {local, 'AF_LOCAL'},
  {inet, 'AF_INET'},
  {ax25, 'AF_AX25'},
  {ipx, 'AF_IPX'},
  {appletalk, 'AF_APPLETALK'},
  {netrom, 'AF_NETROM'},
  {bridge, 'AF_BRIDGE'},
  {atmpvc, 'AF_ATMPVC'},
  {x25, 'AF_X25'},
  {inet6, 'AF_INET6'},
  {rose, 'AF_ROSE'},
  {decnet, 'AF_DECNET'},
  {netbeui, 'AF_NETBEUI'},
  {security, 'AF_SECURITY'},
  {key, 'AF_KEY'},
  {netlink, 'AF_NETLINK'},
  {packet, 'AF_PACKET'},
  {ash, 'AF_ASH'},
  {econet, 'AF_ECONET'},
  {atmsvc, 'AF_ATMSVC'},
  {rds, 'AF_RDS'},
  {sna, 'AF_SNA'},
  {irda, 'AF_IRDA'},
  {pppox, 'AF_PPPOX'},
  {wanpipe, 'AF_WANPIPE'},
  {llc, 'AF_LLC'},
  {can, 'AF_CAN'},
  {tipc, 'AF_TIPC'},
  {bluetooth, 'AF_BLUETOOTH'},
  {iucv, 'AF_IUCV'},
  {rxrpc, 'AF_RXRPC'},
  {isdn, 'AF_ISDN'},
  {phonet, 'AF_PHONET'},
  {ieee802154, 'AF_IEEE802154'}
]}.

{enum, protocol,
 [
  {ip, 'PROTO_IP'},
  {icmp, 'PROTO_ICMP'},
  {igmp, 'PROTO_IGMP'},
  {ipip, 'PROTO_IPIP'},
  {tcp, 'PROTO_TCP'},
  {egp, 'PROTO_EGP'},
  {pup, 'PROTO_PUP'},
  {udp, 'PROTO_UDP'},
  {idp, 'PROTO_IDP'},
  {tp, 'PROTO_TP'},
  {dccp, 'PROTO_DCCP'},
  {ipv6, 'PROTO_IPV6'},
  {routing, 'PROTO_ROUTING'},
  {fragment, 'PROTO_FRAGMENT'},
  {rsvp, 'PROTO_RSVP'},
  {gre, 'PROTO_GRE'},
  {esp, 'PROTO_ESP'},
  {ah, 'PROTO_AH'},
  {icmpv6, 'PROTO_ICMPV6'},
  {none, 'PROTO_NONE'},
  {dstopts, 'PROTO_DSTOPTS'},
  {mtp, 'PROTO_MTP'},
  {encap, 'PROTO_ENCAP'},
  {pim, 'PROTO_PIM'},
  {comp, 'PROTO_COMP'},
  {sctp, 'PROTO_SCTP'},
  {udplite, 'PROTO_UDPLITE'},
  {raw, 'PROTO_RAW'}
]}.

{enum, arphrd, 
 [
  {netrom, 'ARPHRD_NETROM'},
  {ether , 'ARPHRD_ETHER '},
  {eether, 'ARPHRD_EETHER'},
  {ax25, 'ARPHRD_AX25'},
  {pronet, 'ARPHRD_PRONET'},
  {chaos, 'ARPHRD_CHAOS'},
  {ieee802, 'ARPHRD_IEEE802'},
  {arcnet, 'ARPHRD_ARCNET'},
  {appletlk, 'ARPHRD_APPLETLK'},
  {dlci, 'ARPHRD_DLCI'},
  {atm, 'ARPHRD_ATM'},
  {metricom, 'ARPHRD_METRICOM'},
  {ieee1394, 'ARPHRD_IEEE1394'},
  {eui64, 'ARPHRD_EUI64'},
  {infiniband, 'ARPHRD_INFINIBAND'},
  {slip, 'ARPHRD_SLIP'},
  {cslip, 'ARPHRD_CSLIP'},
  {slip6, 'ARPHRD_SLIP6'},
  {cslip6, 'ARPHRD_CSLIP6'},
  {rsrvd, 'ARPHRD_RSRVD'},
  {adapt, 'ARPHRD_ADAPT'},
  {rose, 'ARPHRD_ROSE'},
  {x25, 'ARPHRD_X25'},
  {hwx25, 'ARPHRD_HWX25'},
  {can, 'ARPHRD_CAN'},
  {ppp, 'ARPHRD_PPP'},
  {hdlc, 'ARPHRD_HDLC'},
  {lapb, 'ARPHRD_LAPB'},
  {ddcmp, 'ARPHRD_DDCMP'},
  {rawhdlc, 'ARPHRD_RAWHDLC'},
  {tunnel, 'ARPHRD_TUNNEL'},
  {tunnel6, 'ARPHRD_TUNNEL6'},
  {frad, 'ARPHRD_FRAD'},
  {skip, 'ARPHRD_SKIP'},
  {loopback, 'ARPHRD_LOOPBACK'},
  {localtlk, 'ARPHRD_LOCALTLK'},
  {fddi, 'ARPHRD_FDDI'},
  {bif, 'ARPHRD_BIF'},
  {sit, 'ARPHRD_SIT'},
  {ipddp, 'ARPHRD_IPDDP'},
  {ipgre, 'ARPHRD_IPGRE'},
  {pimreg, 'ARPHRD_PIMREG'},
  {hippi, 'ARPHRD_HIPPI'},
  {ash, 'ARPHRD_ASH'},
  {econet, 'ARPHRD_ECONET'},
  {irda, 'ARPHRD_IRDA'},
  {fcpp, 'ARPHRD_FCPP'},
  {fcal, 'ARPHRD_FCAL'},
  {fcpl, 'ARPHRD_FCPL'},
  {fcfabric, 'ARPHRD_FCFABRIC'},
  {ieee802_tr, 'ARPHRD_IEEE802_TR'},
  {ieee80211, 'ARPHRD_IEEE80211'},
  {ieee80211_prism, 'ARPHRD_IEEE80211_PRISM'},
  {ieee80211_radiotap, 'ARPHRD_IEEE80211_RADIOTAP'},
  {ieee802154, 'ARPHRD_IEEE802154'},
  {phonet, 'ARPHRD_PHONET'},
  {phonet_pipe, 'ARPHRD_PHONET_PIPE'},
  {caif, 'ARPHRD_CAIF'},
  {void, 'ARPHRD_VOID'},
  {none, 'ARPHRD_NONE'}
]}.


{enum, nlmsg_type,
 [
  {noop,         'NLMSG_NOOP'},
  {error,        'NLMSG_ERROR'},
  {done,         'NLMSG_DONE'},
  {overrun,      'NLMSG_OVERRUN'},
  {newlink,      'RTM_NEWLINK'},
  {dellink,      'RTM_DELLINK'},
  {getlink,      'RTM_GETLINK'},
  {setlink,      'RTM_SETLINK'},
  {newaddr,      'RTM_NEWADDR'},
  {deladdr,      'RTM_DELADDR'},
  {getaddr,      'RTM_GETADDR'},
  {newroute,     'RTM_NEWROUTE'},
  {delroute,     'RTM_DELROUTE'},
  {getroute,     'RTM_GETROUTE'},
  {newneigh,     'RTM_NEWNEIGH'},
  {delneigh,     'RTM_DELNEIGH'},
  {getneigh,     'RTM_GETNEIGH'},
  {newrule,      'RTM_NEWRULE'},
  {delrule,      'RTM_DELRULE'},
  {getrule,      'RTM_GETRULE'},
  {newqdisc,     'RTM_NEWQDISC'},
  {delqdisc,     'RTM_DELQDISC'},
  {getqdisc,     'RTM_GETQDISC'},
  {newtclass,    'RTM_NEWTCLASS'},
  {deltclass,    'RTM_DELTCLASS'},
  {gettclass,    'RTM_GETTCLASS'},
  {newtfilter,   'RTM_NEWTFILTER'},
  {deltfilter,   'RTM_DELTFILTER'},
  {gettfilter,   'RTM_GETTFILTER'},
  {newaction,    'RTM_NEWACTION'},
  {delaction,    'RTM_DELACTION'},
  {getaction,    'RTM_GETACTION'},
  {newprefix,    'RTM_NEWPREFIX'},
  {getmulticast, 'RTM_GETMULTICAST'},
  {getanycast,   'RTM_GETANYCAST'},
  {newneightbl,  'RTM_NEWNEIGHTBL'},
  {getneightbl,  'RTM_GETNEIGHTBL'},
  {setneightbl,  'RTM_SETNEIGHTBL'},
  {newnduseropt, 'RTM_NEWNDUSEROPT'},
  {newaddrlabel, 'RTM_NEWADDRLABEL'},
  {deladdrlabel, 'RTM_DELADDRLABEL'},
  {getaddrlabel, 'RTM_GETADDRLABEL'},
  {getdcb,       'RTM_GETDCB'},
  {setdcb,       'RTM_SETDCB'}
 ]}.

%% enumeration of flag numbers (up=0, broadcast=1 ...)
%% used as flag then up = (1 << 0) broadcast = (1 << 1)
{enum, iff_flags,
 [
  up,
  broadcast,
  debug,
  loopback,
  pointopoint,
  notrailers,
  running,
  noarp,
  promisc,
  allmulti,
  master,
  slave,
  multicast,
  portsel,
  automedia,
  ynamic,
  lower_up,
  dormant,
  echo
 ]}.

{enum, nlm_flags, 
 [
  {request, 0},
  {multi,   1},
  {ack,     2},
  {echo,    3}
 ]}.

{enum, nlm_get_flags, 
 [
  {request, 0},
  {multi,   1},
  {ack,     2},
  {echo,    3},
  {root,    8},
  {match,   9},
  {atomic,  10}
 ]}.

{enum, nlm_new_flags, 
 [
  {request, 0},
  {multi,   1},
  {ack,     2},
  {echo,    3},
  {replace, 8},
  {excl,    9},
  {create,  10},
  {append,  11}
 ]}.
 
{define, 'IPCTNL_MSG_CT_NEW', 0}.
{define, 'IPCTNL_MSG_CT_GET', 1}.
{define, 'IPCTNL_MSG_CT_DELETE', 2}.
{define, 'IPCTNL_MSG_CT_GET_CTRZERO', 3}.

{enum, ctm_msgtype_netlink, 
 [
  {noop,          'NLMSG_NOOP'},
  {error,         'NLMSG_ERROR'},
  {done,          'NLMSG_DONE'},
  {overrun,       'NLMSG_OVERRUN'}
 ]}.

{define, 'IPCTNL_MSG_EXP_NEW', 0}.
{define, 'IPCTNL_MSG_EXP_GET', 1}.
{define, 'IPCTNL_MSG_EXP_DELETE', 2}.

{enum, ctm_msgtype_ctnetlink, 
 [
  {new,          'IPCTNL_MSG_CT_NEW'},
  {get,          'IPCTNL_MSG_CT_GET'},
  {delete,       'IPCTNL_MSG_CT_DELETE'},
  {get_ctrzero,  'IPCTNL_MSG_CT_GET_CTRZERO'}
 ]}.

{enum, ctm_msgtype_ctnetlink_exp, 
 [
  {new,          'IPCTNL_MSG_EXP_NEW'},
  {get,          'IPCTNL_MSG_EXP_GET'},
  {delete,       'IPCTNL_MSG_EXP_DELETE'}
 ]}.

{attribute, ctnetlink_tuple_ip, 
 [
  {unspec, binary_t},
  {v4_src, ipv4_addr_t},
  {v4_dst, ipv4_addr_t},
  {v6_src, ipv6_addr_t},
  {v6_dst, ipv6_addr_t}
 ]}.

{attribute, ctnetlink_tuple_proto,
 [
  {unspec,      binary_t},
  {num,         {enum,uint8_t,protocol}},
  {src_port,    uint16_t},
  {dst_port,    uint16_t},
  {icmp_id,     uint16_t},
  {icmp_type,   uint8_t},
  {icmp_code,   uint8_t},
  {icmpv6_id,   binary_t},
  {icmpv6_type, binary_t},
  {icmpv6_code, binary_t}
 ]}.

{attribute, ctnetlink_tuple,
 [
  {unspec, binary_t},
  {ip,     ctnetlink_tuple_ip},
  {proto,  ctnetlink_tuple_proto}
 ]}.

{attribute, ctnetlink_nat_seq_adj, 
 [
  {unspec,         binary_t},
  {correction_pos, uint32_t},
  {offset_before,  uint32_t},
  {offset_after,   uint32_t}
 ]}.

{enum, ctnetlink_protoinfo_tcp_state, 
 [
  none,
  syn_sent,
  syn_recv,
  established,
  fin_wait,
  close_wait,
  last_ack,
  time_wait,
  close,
  listen,
  max,
  ignore
 ]}.

{attribute, ctnetlink_protoinfo_tcp, 
 [
  {unspec,          binary_t},
  {state,           {enum,uint8_t,ctnetlink_protoinfo_tcp_state}},
  {wscale_original, uint8_t},
  {wscale_reply,    uint8_t},
  {flags_original,  uint16_t},
  {flags_reply,     uint16_t}
 ]}.


{attribute, ctnetlink_protoinfo,
 [
  {unspec,    binary_t},
  {tcp,       ctnetlink_protoinfo_tcp},
  {dccp,      binary_t}, %% ctnetlink_protoinfo_dccp},
  {sctp,      binary_t}  %% ctnetlink_protoinfo_sctp}
 ]}.

{enum, ctnetlink_status, 
 [
  expected,
  seen_reply,
  assured,
  confirmed,
  src_nat,
  dst_nat,
  seq_adjust,
  src_nat_done,
  dst_nat_done,
  dying,
  fixed_timeout
 ]}.

{attribute, ctnetlink_help, 
 [
  {unspec, binary_t},
  {name,   string_t}
 ]}.

{attribute, ctnetlink_counters, 
 [
  {unspec,    binary_t},
  {packets,   uint64_t},
  {bytes,     uint64_t},
  {packets32, uint32_t},
  {bytes32,   uint32_t}
 ]}.

{attribute, ctnetlink_timestamp,
 [
  {unspec,  binary_t},
  {start,   uint64_t},
  {stop,    uint64_t}
 ]}.

{attribute, ctnetlink_exp_tuple_ip, 
 [
  {unspec, binary_t},
  {v4_src, ipv4_addr_t},
  {v4_dst, ipv4_addr_t},
  {v6_src, ipv6_addr_t},
  {v6_dst, ipv6_addr_t}
 ]}.

{attribute, ctnetlink_exp_tuple_proto, 
 [
  {unspec,      binary_t},
  {num,         {enum,uint8_t,protocol}},
  {src_port,    uint16_t},
  {dst_port,    uint16_t},
  {icmp_id,     uint16_t},
  {icmp_type,   uint8_t},
  {icmp_code,   uint8_t},
  {icmpv6_id,   binary_t},
  {icmpv6_type, binary_t},
  {icmpv6_code, binary_t}
 ]}.

{attribute, ctnetlink_exp_tuple,
 [
  {unspec, binary_t},
  {ip,     ctnetlink_exp_tuple_ip},
  {proto,  ctnetlink_exp_tuple_proto}
 ]}.

{enum, ctnetlink_exp_flags, 
 [
  permanent,
  inactive,
  userspace
 ]}.


{attribute, ctnetlink_exp,
 [
  {unspec,    binary_t},
  {master,    ctnetlink_exp_tuple},
  {tuple,     ctnetlink_exp_tuple},
  {mask,      ctnetlink_exp_tuple},
  {timeout,   uint32_t},
  {id,        uint32_t},
  {help_name, string_t},
  {zone,      uint16_t},
  {flags,     {flags,uint32_t,ctnetlink_exp_flags}}
 ]}.


{attribute, ctnetlink,
 [
  {unspec,            binary_t},
  {tuple_orig,        ctnetlink_tuple},
  {tuple_reply,       ctnetlink_tuple},
  {status,            {flags,uint32_t,ctnetlink_status}},
  {protoinfo,         ctnetlink_protoinfo},
  {help,              ctnetlink_help},
  {nat_src,           binary_t},
  {timeout,           uint32_t},
  {mark,              uint32_t},
  {counters_orig,     ctnetlink_counters},
  {counters_reply,    ctnetlink_counters},
  {use,               uint32_t},
  {id,                uint32_t},
  {nat_dst,           binary_t},
  {tuple_master,      ctnetlink_tuple},
  {nat_seq_adj_orig,  ctnetlink_nat_seq_adj},
  {nat_seq_adj_reply, ctnetlink_nat_seq_adj},
  {secmark,           uint32_t},
  {zone,              uint16_t},
  {secctx,            binary_t},
  {timestamp,         ctnetlink_timestamp}
 ]}.
 

{attribute, rtnetlink_neigh, 
 [
  {unspec,    binary_t},
  {dst,       addr_t},
  {lladdr,    ether_addr_t},
  {cacheinfo, {array,uint32_t}},
  {probes,    uint32_t}
 ]}.

{enum, rtnetlink_rtm_type, 
 [
  unspec,
  unicast,
  local,
  broadcast,
  anycast,
  multicast,
  blackhole,
  unreachable,
  prohibit,
  throw,
  nat,
  xresolve
 ]}.

{enum, rtnetlink_rtm_protocol, 
 [
  {unspec,   0},
  {redirect, 1},
  {kernel,   2},
  {boot,     3},
  {static,   4},
  {gated,    8},
  {ra,       9},
  {mrt,      10},
  {zebra,    11},
  {bird,     12},
  {dnrouted, 13},
  {xorp,     14},
  {ntk,      15},
  {dhcp,     16}
 ]}.

{enum, rtnetlink_rtm_scope, 
 [
  {universe, 0},
  {site,     200},
  {link,     253},
  {host,     254},
  {nowhere,  255}
 ]}.

%% FIXME: should be bit number?
{enum, rtnetlink_rtm_flags, 
 [
  {notify,   16#100},
  {cloned,   16#200},
  {equalize, 16#400},
  {prefix,   16#800}
 ]}.

{enum, rtnetlink_rtm_table, 
 [
  {unspec,  0},
  {compat,  252},
  {default, 253},
  {main,    254},
  {local,   255}
 ]}.

{attribute, rtnetlink_route_metrics, 
 [
  {unspec,     binary_t},
  {lock,       uint32_t},
  {mtu,        uint32_t},
  {window,     uint32_t},
  {rtt,        uint32_t},
  {rttvar,     uint32_t},
  {ssthresh,   uint32_t},
  {cwnd,       uint32_t},
  {advmss,     uint32_t},
  {reordering, uint32_t},
  {hoplimit,   uint32_t},
  {initcwnd,   uint32_t},
  {features,   uint32_t},
  {rto_min,    uint32_t},
  {initrwnd,   uint32_t}
 ]}.

{attribute, rtnetlink_addr, 
 [
  {unspec,    binary_t},
  {address,   addr_t},
  {local,     addr_t},
  {label,     string_t},
  {broadcast, addr_t},
  {anycast,   addr_t},
  {cacheinfo, {array,uint32_t}},
  {multicast, addr_t}
 ]}.

{enum, rtnetlink_link_operstate, 
 [
  unknown,
  notpresent,
  down,
  lowerlayerdown,
  testing,
  dormant,
  up
 ]}.

{enum, rtnetlink_link_linkmode,
 [
  default,
  dormant
 ]}.

{record, if_map,
 [
  {memstart,  uint64_t},
  {memend,    uint64_t},
  {baseaddr,  uint64_t},
  {irq,       uint16_t},
  {dma,       uint8_t},
  {port,      uint8_t}
  %% fixme: 64 padding? on 64 bit machine?
 ]}.

{attribute, rtnetlink_route, 
 [
  {unspec,    binary_t},
  {dst,       addr_t},
  {src,       addr_t},
  {iif,       uint32_t},
  {oif,       uint32_t},
  {gateway,   addr_t},
  {priority,  uint32_t},
  {prefsrc,   addr_t},
  {metrics,   {tlvs,rtnetlink_route_metrics}},
  {multipath, binary_t},
  {protoinfo, binary_t},
  {flow,      uint32_t},
  {cacheinfo, {array,uint32_t}},
  {session,   binary_t},
  {mp_algo,   binary_t},
  {table,     uint32_t}
 ]}.

{attribute, rtnetlink_link_linkinfo, 
 [
  {unspec, binary_t},
  {kind,   string_t},
  {data,   binary_t},
  {xstats, binary_t}
 ]}.

%% inet6?
{enum, rtnetlink_link_protinfo_flags, 
 [
  {rs_sent,     4},
  {ra_rcvd,     5},
  {ra_managed,  6},
  {ra_othercon, 7},
  {ready,       31}
 ]}.

%% inet6?
{attribute, rtnetlink_link_protinfo,
 [
  {unspec,      binary_t},
  {flags,       {flags,uint32_t,rtnetlink_link_protinfo_flags}},
  {conf,        {array,int32_t}},
  {stats,       {array,uint64_t}},
  {mcast,       binary_t},
  {cacheinfo,   {array,uint32_t}},
  {icmp6stats,  {array,uint64_t}}
 ]}.


{attribute, rtnetlink_link, 
 [
  {unspec,      binary_t},
  {address,     ether_addr_t},
  {broadcast,   ether_addr_t},
  {ifname,      string_t},
  {mtu,         uint32_t},
  {link,        uint32_t},
  {qdisc,       string_t},
  {stats,       {array,uint32_t}},
  {cost,        binary_t},
  {priority,    binary_t},
  {master,      binary_t},
  {wireless,    binary_t},
  {protinfo,    {tlvs,rtnetlink_link_protinfo}},
  {txqlen,      uint32_t},
  {map,         if_map},
  {weight,      binary_t},
  {operstate,   {enum,uint8_t,rtnetlink_link_operstate}},
  {linkmode,    {enum,uint8_t,rtnetlink_link_linkmode}},
  {linkinfo,    {tlvs,rtnetlink_link_linkinfo}},
  {net_ns_pid,  binary_t},
  {ifalias,     string_t},
  {num_vf,      uint32_t},
  {vfinfo_list, binary_t},
  {stats64,     {array,uint64_t}},
  {vf_ports,    binary_t},
  {port_self,   binary_t},
  {af_spec,     binary_t},
  {group,       uint32_t},  %% Group the device belongs to
  {net_ns_fd,   binary_t},
  {ext_mask,    binary_t},  %% Extended info mask, VFs, etc
  {promiscuity, uint32_t}   %%  Promiscuity count: > 0 means acts PROMISC 

 ] }.

{attribute, rtnetlink_prefix, 
 [
  {unspec,    binary_t},
  {address,   addr_t},
  {cacheinfo, {array,uint32_t}}  %% fixme: add struct rta_cacheinfo
 ]}.

{enum, ifa_flags,
 [
  secondary,
  nodad,
  optimistic,
  dadfailed,
  homeaddress,
  deprecated,
  tentative,
  permanent
 ]}.

{record, nlmsghdr,
 [
  {len,    uint32_t},
  {type,   uint16_t},
  {flags,  uint16_t},
  {seq,    uint32_t},  
  {pid,    uint32_t}
 ]}.

{record, ifaddrmsg,
 [
  {family,     {enum,uint8_t,family}},
  {prefixlen,  uint8_t},
  {flags,      {flags,uint8_t,ifa_flags}},
  {scope,      uint8_t},
  {index,      uint32_t},
  {attributes, {tlvs,rtnetlink_addr}}
 ]}.

{record, newaddr, ifaddrmsg}.
{record, deladdr, ifaddrmsg}.
{record, getaddr, ifaddrmsg}.

{record, ifinfomsg,
 [
  {family,     {enum,uchar_t,family}},
  {'_',        uchar_t},
  {arphrd,     {enum,ushort_t,arphrd}},
  {index,      int_t},
  {flags,      {flags,uint_t,iff_flags}},
  {change,     {flags,uint_t,iff_flags}},
  {attributes, {tlvs,rtnetlink_link}}
 ]}.

{record, newlink, ifinfomsg}.
{record, dellink, ifinfomsg}.
{record, getlink, ifinfomsg}.

{record, rtmsg,
 [
  {family,     {enum,uchar_t,family}},
  {dstlen,     uchar_t},
  {srclen,     uchar_t},
  {tos,        uchar_t},
  {table,      uchar_t},  %% routing table id
  {protocol,   {enum,uchar_t,protocol}},
  {scope,      uchar_t},
  {rtm_type,   uchar_t},
  {flags,      uint_t},
  {attributes, {tlvs, rtnetlink_route}}
 ]}.

{record, newroute, rtmsg}.
{record, delroute, rtmsg}.
{record, getroute, rtmsg}.

{record, ndmsg, 
 [
  {family,     {enum,uint8_t,family}},
  {'_',        uint8_t},
  {'_',        uint16_t},
  {index,      uint32_t},
  {state,      uint16_t},
  {flags,      uint8_t},
  {nmd_type,   uint8_t},
  {attributes, {tlvs, rtnetlink_neigh}}
 ]}.
  
{record, newneigh, ndmsg}.
{record, delneigh, ndmsg}.
{record, getneigh, ndmsg}.

%% newrule, delrule,   getrule
%% newdisc, deldisc,   getdisc
%% newtclass, deltclass, gettclass
%% newtfilter, deltfilter, gettfilter
%% newaction, delaction, getaction
%% newprefix, delprefix
%% newneightbl, getneightbl, setneightbl
%% newnduseropt
%% newaddrlabel
%% getdcb, setdcb

{record, noop, 
 [
 ]}.

{record, error, 
 [
  {errno,  int_t},
  {msg,    nlmsghdr},  %% the original header
  {data,   binary_t}   %% truncated data?
 ]}.

{record, done,
 [
  {status, uint32_t}  %% ? int_t?
 ]}.

{record, overrun, 
 [
  {status, uint32_t}  %% ? int_t?
 ]}.
