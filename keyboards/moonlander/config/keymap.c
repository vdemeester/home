/* Copyright 2020 ZSA Technology Labs, Inc <@zsa>
 * Copyright 2020 Jack Humbert <jack.humb@gmail.com>
 * Copyright 2020 Christopher Courtney, aka Drashna Jael're  (@drashna) <drashna@live.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */



#include QMK_KEYBOARD_H
#include "version.h"
#include "keymap_us.h"
#include "keymap_bepo.h"

enum layers {
    QWER,
    BEPO,
    ERGL,
    SYMB,
    NUMB,
    NAVI,
    MOUS,
};

enum custom_keycodes {
    VRSN = SAFE_RANGE,
};

const key_override_t *key_overrides[] = {
};

combo_t key_combos[] = {
};

tap_dance_action_t tap_dance_actions[] = {
};

// clang-format off
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [QWER] = LAYOUT(
        KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
        KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
        KC_EQL,  GUI_A,    ALT_S,    SFT_D,    CTL_F,    HYP_G,    XXXXXXX,           XXXXXXX, HYP_H,    CTL_J,    SFT_K,    ALT_L,    GUI_SCLN, KC_QUOT,
        KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
        XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  _______,               _______,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
	LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  SFTLLCK,  LT(SYMB, KC_ENT)
    ),
    [BEPO] = LAYOUT(
        KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
        KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
        // KC_TAB,  KC_B,    KC_É,    KC_P,    KC_O,    KC_È,    XXXXXXX,           XXXXXXX, caret,    KC_V,    KC_D,    KC_L,    KC_J,    KC_Z,
        KC_EQL,  KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    XXXXXXX,           XXXXXXX,  KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
        // KC_EQL,  KC_A,    KC_U,    KC_I,    KC_E,    KC_COM,  XXXXXXX,           XXXXXXX,  KC_H,    KC_J,    KC_K,    KC_L,    LT(NAVI, KC_SCLN), LGUI_T(KC_QUOT),
        KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
        //KC_GRV, LCTL_T(KC_Z),KC_X,KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  RCTL_T(KC_SLSH), KC_RSFT,
        XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  _______,               _______,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
	LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  SFTLLCK,  LT(SYMB, KC_ENT)
    ),
    [ERGL] = LAYOUT(
        KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
        KC_DEL,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_BSLS,
        KC_BSPC, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    XXXXXXX,           XXXXXXX,  KC_H,    KC_J,    KC_K,    KC_L,    LT(NAVI, KC_SCLN), LGUI_T(KC_QUOT),
        KC_LSFT, LCTL_T(KC_Z),KC_X,KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  RCTL_T(KC_SLSH), KC_RSFT,
        XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  _______,               _______,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
	LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  SFTLLCK,  LT(SYMB, KC_ENT)
    ),

    [SYMB] = LAYOUT(
        VRSN,    _______,   _______,   _______,   _______,   _______,   _______,           _______, _______,  _______,  _______,  _______, _______,  _______,
        KC_BSLS, KC_EXLM, KC_AT,   KC_LCBR, KC_RCBR, KC_PIPE, _______,           _______, KC_UP,   KC_7,    KC_8,    KC_9,    KC_ASTR, KC_F12,
        _______, KC_EXLM, KC_DLR,  KC_LPRN, KC_RPRN, KC_GRV,  _______,           _______, KC_DOWN, KC_4,    KC_5,    KC_6,    KC_PLUS, _______,
        _______, KC_TILD, KC_CIRC, KC_LBRC, KC_RBRC, KC_TILD,                             KC_AMPR, KC_1,    KC_2,    KC_3,    KC_BSLS, _______,
        EE_CLR,  _______, _______, _______, _______,          RM_VALU,           RM_TOGG,          _______, KC_DOT,  KC_0,    KC_EQL,  _______,
                                            RM_HUED, RM_VALD, RM_HUEU, TOGGLE_LAYER_COLOR,_______, _______
    ),
    [NUMB] = LAYOUT(
        VRSN,    _______,   _______,   _______,   _______,   _______,   _______,           _______, _______,  _______,  _______,  _______, _______,  _______,
        _______, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   _______,           _______, _______, KC_7,    KC_8,    KC_9,    KC_PAST, KC_PSLS,
        _______, KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  _______,           _______, _______, KC_4,    KC_5,    KC_6,    KC_PMNS, KC_PPLS,
        _______, KC_F11,  KC_F12,  KC_F13,  KC_F14,  KC_F15,                              _______, KC_1,    KC_2,    KC_3,    KC_PDOT, KC_PCMM,
        EE_CLR,  _______, _______, _______, _______,          _______,           _______,          _______, _______, _______, KC_0,    _______,
                                            _______, _______, _______,           _______, _______, _______
    ),
    [NAVI] = LAYOUT(
        VRSN,    _______,   _______,   _______,   _______,   _______,   _______,           _______, _______,  _______,  _______,  _______, _______,  QK_BOOT,
        _______, _______, KC_AT,   KC_LCBR, KC_RCBR, KC_PIPE,  _______,           _______, KC_PSCR,   KC_HOME,    KC_UP,    KC_END,    KC_PGUP, _______,
        _______, KC_HASH, KC_MPRV, KC_MPLY, KC_MNXT, _______,  _______,           _______, _______, KC_LEFT,    KC_DOWN,    KC_RGHT,    KC_PGDN, _______,
        _______, _______, _______, _______, _______, _______,                              KC_MUTE, _______, _______, _______, _______, _______,
        EE_CLR,  _______, _______, _______, _______,          KC_VOLD,           KC_VOLU,           _______, _______, _______, _______,  _______,
                                            _______, _______, _______,           _______, _______, _______
    ),

    [MOUS] = LAYOUT(
        LED_LEVEL,_______,_______, _______, _______, _______, _______,           _______, _______, _______, _______, _______, _______, QK_BOOT,
        _______, _______, _______, _______,   _______, _______, _______,           _______, _______, _______, MS_UP, _______, _______, _______,
        _______, MS_WHLL, MS_WHLU, MS_WHLD, MS_WHLR, _______, _______,           _______, _______, MS_LEFT, MS_DOWN, MS_RGHT, _______, _______,
        _______, _______, _______, _______, _______, _______,                              _______, _______, _______, _______, _______, _______,
        EE_CLR,  _______, _______, _______, _______,          KC_VOLD,           KC_VOLU,           _______, _______, _______, _______,  _______,
                                            _______, MS_BTN1, MS_BTN3,           MS_BTN3, MS_BTN2, _______
    ),
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (record->event.pressed) {
        switch (keycode) {
        case VRSN:
            SEND_STRING (QMK_KEYBOARD "/" QMK_KEYMAP " @ " QMK_VERSION);
            return false;
        case SFTLLCK:
            if (record->tap.count) {
                if (record->event.pressed) {
                    // Toggle the lock on the highest layer.
                    layer_lock_invert(get_highest_layer(layer_state));
                }
                return false;
            }
	    break;
	}
    }
    return true;
}

extern rgb_config_t rgb_matrix_config;

// Define common colors as RGB values
#define RGB_OFF 0, 0, 0
#define RGB_ORANGE_YELLOW 213, 144, 0
#define RGB_GREEN 27, 213, 0
#define RGB_GREEN_LIGHT 206, 255, 206
#define RGB_CYAN 0, 255, 167
#define RGB_BLUE_LIGHT 0, 113, 255
#define RGB_BLUE 0, 55, 255
#define RGB_BLUE_BRIGHT 0, 183, 255
#define RGB_WHITE 255, 255, 255
#define RGB_YELLOW 255, 199, 0
#define RGB_YELLOW_GREEN 183, 255, 0
#define RGB_PURPLE 13, 0, 255
#define RGB_GREEN_MEDIA 27, 213, 0
#define RGB_PURPLE_VOL 170, 0, 255

// Shorthand for RGB values - _ means RGB_OFF
#define ___ {RGB_OFF}

// LED_LAYOUT macro - matches the visual representation of the keyboard LAYOUT
// This makes it easy to see which keys have which colors
#define LED_LAYOUT( \
    k00, k05, k10, k15, k20, k25, k29,           k65, k61, k56, k51, k46, k41, k36, \
    k01, k06, k11, k16, k21, k26, k30,           k66, k62, k57, k52, k47, k42, k37, \
    k02, k07, k12, k17, k22, k27, k31,           k67, k63, k58, k53, k48, k43, k38, \
    k03, k08, k13, k18, k23, k28,                     k64, k59, k54, k49, k44, k39, \
    k04, k09, k14, k19, k24,      k35,           k71,      k60, k55, k50, k45, k40, \
                        k32, k33, k34,           k68, k69, k70 \
) { \
    k00, k01, k02, k03, k04, k05, k06, k07, k08, k09, \
    k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, \
    k20, k21, k22, k23, k24, k25, k26, k27, k28, k29, \
    k30, k31, k32, k33, k34, k35, k36, k37, k38, k39, \
    k40, k41, k42, k43, k44, k45, k46, k47, k48, k49, \
    k50, k51, k52, k53, k54, k55, k56, k57, k58, k59, \
    k60, k61, k62, k63, k64, k65, k66, k67, k68, k69, \
    k70, k71 \
}

// LED map for each layer - using LED_LAYOUT for easy visualization
const uint8_t PROGMEM ledmap[][72][3] = {
    [QWER] = LED_LAYOUT(
        ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, ___,                           ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW},
        {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, ___,                           ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW},
        {RGB_GREEN},     {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN},         ___,                           ___,             {RGB_GREEN},         {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN},
        {RGB_CYAN},      {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},                                                          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},
        ___,             ___,                 ___,                 ___,                 {RGB_BLUE_LIGHT},                         {RGB_BLUE_LIGHT},              {RGB_BLUE_LIGHT},                     {RGB_BLUE_LIGHT},    ___,                 ___,                 ___,                 ___,
                                                                   {RGB_BLUE_LIGHT},    {RGB_BLUE_LIGHT},    ___,                           ___,             {RGB_BLUE_LIGHT},    {RGB_BLUE_LIGHT}
    ),
    [NUMB] = LED_LAYOUT(
        {RGB_RED},       ___,        ___,        ___,        ___,        ___,        ___,                           ___,        ___,        ___,        ___,        ___,        ___,        ___,
        ___,             {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, ___,                           ___,        ___,        ___,        {RGB_YELLOW}, {RGB_YELLOW}, {RGB_YELLOW}, {RGB_YELLOW_GREEN},
        ___,             {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, ___,                           ___,        ___,        ___,        {RGB_YELLOW}, {RGB_YELLOW}, {RGB_YELLOW}, {RGB_YELLOW_GREEN},
        ___,             {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE_BRIGHT}, {RGB_BLUE_BRIGHT}, {RGB_BLUE_BRIGHT},                                   ___,        ___,        {RGB_YELLOW}, {RGB_YELLOW}, {RGB_YELLOW}, {RGB_YELLOW_GREEN},
        ___,             ___,        ___,        ___,        ___,                    ___,                           ___,                    {RGB_WHITE}, ___,        ___,        ___,        ___,
                                                 ___,        ___,        ___,                           ___,        ___,        ___
    ),
    [SYMB] = LED_LAYOUT(
        ___,        ___,        ___,        ___,        ___,        ___,        ___,                           ___,        ___,        ___,        ___,        ___,        ___,        ___,
        ___,        ___,        {RGB_YELLOW}, {RGB_YELLOW}, ___,      ___,        ___,                           ___,        ___,        {RGB_YELLOW}, {RGB_YELLOW}, ___,        ___,        ___,
        ___,        ___,        ___,        ___,        ___,        ___,        ___,                           ___,        ___,        {RGB_YELLOW}, {RGB_YELLOW}, ___,        ___,        ___,
        ___,        ___,        ___,        {RGB_YELLOW}, {RGB_YELLOW}, ___,                                               ___,        ___,        ___,        ___,        ___,        ___,
        ___,        ___,        ___,        ___,        ___,                    ___,                           ___,                    ___,        ___,        ___,        ___,        ___,
                                            ___,        ___,        ___,                           ___,        ___,        ___
    ),
    [NAVI] = LED_LAYOUT(
        {RGB_RED},      ___,        ___,        ___,        ___,        ___,        ___,                           ___,        ___,        ___,        ___,        ___,        ___,        {RGB_RED},
        ___,            ___,        ___,        ___,        ___,        ___,        ___,                           ___,        {RGB_WHITE}, ___,        {RGB_BLUE_LIGHT}, {RGB_PURPLE}, {RGB_PURPLE}, ___,
        ___,            ___,        {RGB_GREEN_MEDIA}, ___,  ___,        ___,        ___,                           ___,        ___,        ___,        {RGB_BLUE_LIGHT}, {RGB_BLUE_LIGHT}, {RGB_PURPLE}, ___,
        ___,            ___,        ___,        {RGB_GREEN_MEDIA}, {RGB_GREEN_MEDIA}, ___,                                   {RGB_PURPLE_VOL}, ___,     ___,        {RGB_BLUE_LIGHT}, ___,        ___,
        ___,            ___,        ___,        ___,        ___,                    {RGB_PURPLE_VOL},              {RGB_PURPLE_VOL},        ___,        ___,        ___,        ___,        ___,
                                                ___,        ___,        ___,                           ___,        ___,        ___
    ),
};

void set_layer_color(int layer) {
    for (int i = 0; i < 72; i++) {
        uint8_t r = pgm_read_byte(&ledmap[layer][i][0]);
        uint8_t g = pgm_read_byte(&ledmap[layer][i][1]);
        uint8_t b = pgm_read_byte(&ledmap[layer][i][2]);
        rgb_matrix_set_color(i, r, g, b);
    }
}

// matrix, to set key with. maybe there is a better way but used ascii generator so i can easily set those visually
//    ┌────┬────┬────┬────┬────┬────┬────┐             ┌────┬────┬────┬────┬────┬────┬────┐
//    │ 0  │ 5  │ 10 │ 15 │ 20 │ 25 │ 29 │             │ 65 │ 61 │ 56 │ 51 │ 46 │ 41 │ 36 │
//    ├────┼────┼────┼────┼────┼────┼────┤             ├────┼────┼────┼────┼────┼────┼────┤
//    │ 1  │ 6  │ 11 │ 16 │ 21 │ 26 │ 30 │             │ 66 │ 62 │ 57 │ 52 │ 47 │ 42 │ 37 │
//    ├────┼────┼────┼────┼────┼────┼────┤             ├────┼────┼────┼────┼────┼────┼────┤
//    │ 2  │ 7  │ 12 │ 17 │[22]│ 27 │ 31 │             │ 67 │ 63 │[58]│ 53 │ 48 │ 43 │ 38 │
//    ├────┼────┼────┼────┼────┼────┼────┘             └────┼────┼────┼────┼────┼────┼────┤
//    │ 3  │ 8  │ 13 │ 18 │ 23 │ 28 │                       │ 64 │ 59 │ 54 │ 49 │ 44 │ 39 │
//    ├────┼────┼────┼────┼────┼────┼─────────┐   ┌─────────┼────┼────┼────┼────┼────┼────┤
//    │ 4  │ 9  │ 14 │ 19 │ 24 │    │   35    │   │   71    │    │ 60 │ 55 │ 50 │ 45 │ 40 │
//    └────┴────┴────┴────┴────┼────┼────┬────┤   ├────┬────┼────┼────┴────┴────┴────┴────┘
//                             │ 32 │ 33 │ 34 │   │ 68 │ 69 │ 70 │
//                             └────┴────┴────┘   └────┴────┴────┘
// Runs constantly in the background, in a loop.
bool rgb_matrix_indicators_user(void) {
  if (rgb_matrix_get_flags() & (LED_FLAG_KEYLIGHT | LED_FLAG_MODIFIER)) {
    uint8_t layer = get_highest_layer(layer_state);
    switch (layer) {
    case QWER:
    case NUMB:
    case SYMB:
    case NAVI:
      rgb_matrix_set_color_all(RGB_OFF);
      set_layer_color(layer);
      break;
    case MOUS:
      // MOUS layer has no custom LED configuration
      break;
    default:
      rgb_matrix_set_color_all(RGB_OFF);
      break;
    }
  }
  return false;
}
