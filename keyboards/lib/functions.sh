#!/usr/bin/env bash
# Author: Chmouel Boudjnah <chmouel@chmouel.com>

if [[ -e config/includes/local.h ]]; then
  cat <<EOF >config/includes/local.h
#define MYDEBUG_PASTE_MACRO &kp D &kp E &kp B &kp U &kp G
#define MYDEBUG_PASTE_MACRO_2 &kp STAR &kp SPACE &kp D &kp E &kp B &kp U &kp G &kp SPACE &kp STAR
EOF
fi

function echo_red() {
  echo -e "\033[0;31m$1\033[0m"
}

function echo_green() {
  echo -e "\033[0;32m$1\033[0m"
}

function echo_blue() {
  echo -e "\033[0;34m$1\033[0m"
}

function echo_yellow() {
  echo -e "\033[0;33m$1\033[0m"
}
