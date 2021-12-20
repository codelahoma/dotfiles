#!/usr/bin/env python3
import json
import subprocess

SALTMASTER_IP = "10.8.4.70"
args = [
    "ssh",
    SALTMASTER_IP,
    "sudo",
    "salt",
    "saltmaster-01",
    "mine.get",
    "'*'",
    "vpn_ip",
    "--out",
    "json",
]
completed = subprocess.run(args, stdout=subprocess.PIPE)
salt_out = json.loads(completed.stdout)
hosts = salt_out["saltmaster-01"]
for k, v in hosts.items():
    try:
        print(f"Host {k}\n    HostName {v[0]}\n")
    except:
        pass
