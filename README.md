# ovpn-mode
An openvpn management mode for Emacs24+ that lets you start|stop|restart openvpn configurations.

![ovpn-mode screenshot](ovpn-mode.png?raw=true "ovpn-mode")

You can see a demo of the UI in action at https://www.youtube.com/watch?v=kIs8wSWXIHE

## general usage
`M-x ovpn` will pop you into the default configuration directory and list any existing .ovpn files from there. You can then interact with that listing with the following single key commands:

- `s`: start the selected ovpn
- `n`: start the selected ovpn in a dedicated namespace
- `q`: stop the selected ovpn
- `r`: restart the selected ovpn

Additionally you have available:

- `i`: remote link info for the selected ovpn
- `b`: switch to the output buffer for the selected ovpn
- `e`: edit the selected ovpn
- `d`: set the active vpn conf directory
- `6`: toggle ipv6 support on/off (automatically called on start of ovpn)
- `x`: execute an asynchronous shell command in the context of any associated namespace
- `a`: show all active vpn configurations accross all conf directories
- `h`: describe mode

`M-x ovpn-mode-dir-set` lets you point ovpn-mode at any additional directories. ovpn-mode will maintain state for any running configurations, so you can switch between multiple directories and keep state accordingly.

## namespace integration

This is currently in beta. By starting a configuration with `n` as opposed to `s` you will set up a dedicated network namespace for the openvpn instance to run inside of. Once this namespace is initialized, you can then use `x` to execute commands as a specified user from within that namespace. You can do this for multiple concurrent vpn connections, without affecting your existing main networking routes.

This is convenient to e.g. isolate a specific process to a certain vpn without having to do a bunch of routing. Personally I just spawn an xterm from a namespace and then do whatever I want for that specific vpn instance from that xterm (e.g. start rtorrent, an incognito browser session, etc.)

Namespace instances will automagically drop the default route for the namespace as soon as the vpn connection is fully initialized. This prevents any process being able to connect out via your default system route (i.e. your real IP address) if a VPN configuration were to fail.

To set the DNS servers to use in these namespaces you can alter:

```lisp
(defvar ovpn-mode-netns-ns0 "8.8.8.8") ; ns1 to use in namespace
(defvar ovpn-mode-netns-ns1 "8.8.4.4") ; ns2 to use in namespace
```

This work was inspired by crasm's vpnshift.sh script (https://github.com/vpnshift.sh) but implemented in elisp and enhanced to allow for concurrent namespaces and multiple vpns running at the same time, with minimal user configuration overhead.

## configuration

After writing the initial version, I really started to dislike having to type my sudo password all the time, so I've included some logic for automatic sudo authentication for people that want it.
Please note that due to needing synchronous execution of configuration commands, this is not enabled for namespace initialization, and you _will_ be prompted for your sudo password by tramp for
that.

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

