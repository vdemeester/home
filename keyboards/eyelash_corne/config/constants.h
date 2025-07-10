#pragma once

// Keys
#define ___ &trans
#define XXX &none

// Layers
#define BEP 0
/* #define BAC 1 */
#define ERG 1
#define EAC 2
#define QWE 3
#define QAC 4
#define SYM 5
#define NAV 6
#define NUM 7
#define MOU 8
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
