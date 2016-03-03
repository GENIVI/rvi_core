
%% netlink protocols
-define(NETLINK_ROUTE,		0).  %% Routing/device hook
-define(NETLINK_UNUSED,		1).  %% Unused number
-define(NETLINK_USERSOCK,	2).  %% Reserved for user mode socket protocols
-define(NETLINK_FIREWALL,	3).  %% Unused number, formerly ip_queue
-define(NETLINK_SOCK_DIAG,	4).  %% socket monitoring
-define(NETLINK_NFLOG,		5).  %% netfilter/iptables ULOG */
-define(NETLINK_XFRM,		6).  %% ipsec */
-define(NETLINK_SELINUX,	7).  %% SELinux event notifications */
-define(NETLINK_ISCSI,		8).  %% Open-iSCSI */
-define(NETLINK_AUDIT,		9).  %% auditing */
-define(NETLINK_FIB_LOOKUP,	10).	
-define(NETLINK_CONNECTOR,	11).
-define(NETLINK_NETFILTER,	12).  %% netfilter subsystem
-define(NETLINK_IP6_FW,		13).
-define(NETLINK_DNRTMSG,	14).  %% DECnet routing messages
-define(NETLINK_KOBJECT_UEVENT,	15).  %% Kernel messages to userspace
-define(NETLINK_GENERIC,	16).
%% leave room for NETLINK_DM (DM Events)
-define(NETLINK_SCSITRANSPORT,	18).  %% SCSI Transports
-define(NETLINK_ECRYPTFS,	19).
-define(NETLINK_RDMA,		20).
-define(NETLINK_CRYPTO,		21).	%% Crypto layer */
-define(NETLINK_INET_DIAG,	?NETLINK_SOCK_DIAG).

-define(NFNLGRP_NONE,                  0).
-define(NFNLGRP_CONNTRACK_NEW,         1).
-define(NFNLGRP_CONNTRACK_UPDATE,      2).
-define(NFNLGRP_CONNTRACK_DESTROY,     3).
-define(NFNLGRP_CONNTRACK_EXP_NEW,     4).
-define(NFNLGRP_CONNTRACK_EXP_UPDATE,  5).
-define(NFNLGRP_CONNTRACK_EXP_DESTROY, 6).

-define(RTNLGRP_NONE,          0).
-define(RTNLGRP_LINK,          1).
-define(RTNLGRP_NOTIFY,        2).
-define(RTNLGRP_NEIGH,         3).
-define(RTNLGRP_TC,            4).
-define(RTNLGRP_IPV4_IFADDR,   5).
-define(RTNLGRP_IPV4_MROUTE,   6).
-define(RTNLGRP_IPV4_ROUTE,    7).
-define(RTNLGRP_IPV4_RULE,     8).
-define(RTNLGRP_IPV6_IFADDR,   9).
-define(RTNLGRP_IPV6_MROUTE,   10).
-define(RTNLGRP_IPV6_ROUTE,    11).
-define(RTNLGRP_IPV6_IFINFO,   12).
-define(RTNLGRP_DECnet_IFADDR, 13).
-define(RTNLGRP_NOP2,          14).
-define(RTNLGRP_DECnet_ROUTE,  15).
-define(RTNLGRP_DECnet_RULE,   16).
-define(RTNLGRP_NOP4,          17).
-define(RTNLGRP_IPV6_PREFIX,   18).
-define(RTNLGRP_IPV6_RULE,     19).
-define(RTNLGRP_ND_USEROPT,    20).
-define(RTNLGRP_PHONET_IFADDR, 21).
-define(RTNLGRP_PHONET_ROUTE,  22).
-define(RTNLGRL_DCB,           23).

-record(nlmsg,
	{
	  hdr,
	  data 
	}).
