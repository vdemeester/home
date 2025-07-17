#pragma once

// Keys
#define ___ &trans
#define XXX &none

// Layers
#define BEP 0
#define ERG 1
#define QWE 2
#define ACC 3
#define SYM 4
#define NAV 5
#define NUM 6
#define MOU 7

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

#define MATH_1_4 RA(N6) // ¼
#define MATH_1_2 RA(N7) // ½
#define MATH_3_4 RA(N8) // ¾

#define MATH_PM    RA(N1)   // 
#define MATH_DEG   RA(SEMI) // °
#define MATH_MICRO RA(M)    // µ
