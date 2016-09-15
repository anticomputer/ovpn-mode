# ovpn-mode
An openvpn management mode for Emacs24+ that lets you start|stop|restart openvpn configurations.

![ovpn-mode screenshot](ovpn-mode.png?raw=true "ovpn-mode")

## general usage
`M-x ovpn` will pop you into the default configuration directory and list any existing .ovpn files from there. You can then interact with that listing with the following single key commands:

- `s`: start the selected ovpn
- `q`: stop the selected ovpn
- `r`: restart the selected ovpn

Additionally you have available:

- `i`: remote link info for the selected ovpn
- `b`: switch to the output buffer for the selected ovpn
- `e`: edit the selected ovpn
- `d`: set the active vpn conf directory
- `6`: toggle ipv6 support on/off (automatically called on start of ovpn)
- `h`: describe mode

`M-x ovpn-mode-dir-set` lets you point ovpn-mode at any additional directories. ovpn-mode will maintain state for any running configurations, so you can switch between multiple directories and keep state accordingly.

## configuration

After writing the initial version, I really started to dislike having to type my sudo password all the time, so I've included some logic for automatic sudo authentication for people that want it.

```lisp
;;; sudo auth convenience functions
(defvar ovpn-mode-authinfo (expand-file-name "~/.authinfo.gpg"))
(defvar ovpn-mode-authinfo-token "ovpn-mode-sudo")
(defvar ovpn-mode-use-authinfo t) ; set to nil if you prefer to be prompted
```

Configure the above according to your local setup, and then add a line like the following to your .authinfo:

`machine ovpn-mode-sudo login root passsword yoursudopasshere`

ovpn-mode will then automagically grab your sudo creds, which makes for smoother sailing in general. Obviously you can also just turn it off if you prefer to just type your password as prompted.

## notes
This is something I wrote to fit my exact use case (i.e. I like to be able to pop into and out of multiple openvpn configurations). It should work on any UNIX-like system that has sudo and openvpn available but I've only tested it on Linux.

