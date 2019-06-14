#!/usr/bin/sh
PROPATH="$(dirname "${0}")"
DLC="/opt/qadee/progress/dlc113"
display_banner=no

PATH="${DLC}/bin:${PATH}"
TERM="${TERM-vt220}"

export DLC PROPATH TERM display_banner
echo $PROPATH
cd "$(dirname "${0}")"
pro -p fe.p -h 20 -T "$(dirname "${0}")"  -cprcodeout utf-8  -v6colon  -cpstream utf-8  #-cpinternal utf-8


