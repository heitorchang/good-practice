# Ubuntu 20.04

## Wi-fi

Wi-fi suddenly stopped working. The fix:

Check Broadcom card specs

`sudo lshw -C network`

Clear bcmwl package and install broadcom-sta-source

`sudo apt-get purge bcmwl-kernel-source`

`sudo apt-get install broadcom-sta-source broadcom-sta-dkms broadcom-sta-common`

Reboot

## Monitoring

Disk: sudo iotop

Network: sudo nethogs

Memory: top -c (shows all processes)

top -c -d 0.5 refreshes every 0.5 seconds

press 'o' then type COMMAND=py (for example, for python processes)

press 'e' to switch to human-readable sizes