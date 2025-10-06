#pragma once
#include "matrix_rgb.h"

enum {
    TD_SCREENSHOT,
    TD_RSHIFT,
};
#define ALT OSM(MOD_LALT)
#define MPLPA A(G(C(S(KC_M))))
#define CSM OSM(MOD_LCTL | MOD_LSFT)
#define CST(x) MT(MOD_LCTL | MOD_LSFT, x)
#define GT LGUI_T
#define CT LCTL_T
#define AT LALT_T
#define RAT RALT_T
#define ST LSFT_T
#define H HYPR

enum layers {
    BASE = 0,
    SYMB,
    MDIA,
    STUFF,
    MOTION,
    GAME,
    ACCENT,
};
enum custom_keycodes { VRSN = SAFE_RANGE, CHMOU_CUSTOM_KEYCODES };
