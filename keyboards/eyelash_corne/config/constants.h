#pragma once

// Keys
#define ___ &trans
#define XXX &none

// Layers
#define BEP 0
#define BAC 1
#define ERG 2
#define EAC 3
#define QWE 4
#define QAC 5
#define SYM 6
#define NAV 7
#define NUM 8
#define MOU 9
// #define SYS 10

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
#define HL LEFT_HYPER
#define LH(keycode)            LS(LA(LC(LG(keycode))))
