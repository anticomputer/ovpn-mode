# ovpn-mode
An openvpn management mode for Emacs24 that lets you start|stop|restart openvpn configurations.

![ovpn-mode screenshot](ovpn-mode.png?raw=true "ovpn-mode")

## general usage
`M-x ovpn` will pop you into the default configuration directory and list any existing .ovpn files from there. You can then interact with that listing with the following single key commands:

- `s`: start the selected ovpn
- `q`: stop the selected ovpn
- `r`: restart the selected ovpn

Additionally you have available:

- `i`: remote link info for the selected ovpn
- `b`: switch to the output buffer for the selected ovpn

`M-x ovpn-mode-dir-set` lets you point ovpn-mode at any additional directories. ovpn-mode will maintain state for any running configurations, so you can switch between multiple directories and keep state accordingly.

This is something I wrote to fit my exact use case (i.e. I like to be able to pop into and out of multiple openvpn configurations). It should work on any UNIX-like system that has sudo and openvpn available but I've only tested it on Linux.
