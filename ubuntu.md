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