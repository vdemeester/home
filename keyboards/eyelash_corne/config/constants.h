#pragma once

// Keys
#define ___ &trans
#define XXX &none

// Layers
#define QWE 0
// #define QAC 1
#define ERG 1
#define EAC 2
#define SYM 3
#define NAV 4
#define NUM 5
#define MOU 6

// Modifier keys
#define GL LGUI
#define CL LCTRL
#define AL LALT
#define SL LSHFT
#define GR RGUI
#define CR RCTRL
#define AR RALT
#define SR RSHFT

// OS
#define WINDOWS 0
#define LINUX 1
#define MAC_OS 2

// Hyper Key and Functions
#define LEFT_HYPER LS(LA(LC(LGUI)))
#define RIGHT_HYPER RS(RA(RC(RGUI)))
#define LH(keycode)            LS(LA(LC(LG(keycode))))
#define RH(keycode)            RS(RA(RC(RG(keycode))))
