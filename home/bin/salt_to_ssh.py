#!/usr/bin/env python3
import json
import subprocess

args = [
    "ssh",
    "10.8.0.6",
    "sudo",
    "salt",
    "saltmaster-3",
    "mine.get",
    "'*'",
    "vpn_ip",
    "--out",
    "json",
]
completed = subprocess.run(args, stdout=subprocess.PIPE)
salt_out = json.loads(completed.stdout)
hosts = salt_out["saltmaster-3"]
for k, v in hosts.items():
    try:
        print(f"Host {k}\n    HostName {v[0]}\n")
    except:
        pass
