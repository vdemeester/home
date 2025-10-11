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
#include "keymap_us_international_linux.h"
#include "layers.h"

#include "key_overrides.h"
#include "combos.h"

tap_dance_action_t tap_dance_actions[] = {
};

#include "macros.h"
#include "rgb_lighting.h"

// clang-format off
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [BEPO] = LAYOUT(
		  // $  " « » ( ) @ + - / * = %
		  // shift: # 1 2 3 4 5 6 7 8 9 0 ° `
		  // ralt : – — < > [ ] ^ ± − ÷ × ≠ ‰
		  // shift+ralt : ¶ „ “ ” ≤ ≥ _ ¬ ¼ ½ ¾ ′ ″
		  // FIXME: should I invert ?
		  KC_DLR,  FR_DQUO,    US_LDAQ,    US_RDAQ,  KC_LPRN,    KC_RPRN,    XXXXXXX,           XXXXXXX, KC_AT,    KC_PLUS,    KC_PMNS,    KC_PSLS,    KC_PAST,    KC_PERC,
		  KC_TAB,  KC_B,    FR_E_AIGU,    KC_P,    KC_O,    FR_E_GRAVE,    XXXXXXX,           XXXXXXX, KC_CIRC,    KC_V,    KC_D,    KC_L,    KC_J,    KC_Z,
		  KC_EQL,  HM_GUI_A,    HM_ALT_U,    HM_SFT_I,    HM_CTL_E,    HM_HYP_COMM,    XXXXXXX,           XXXXXXX, HM_HYP_C,    HM_CTL_T,    HM_SFT_S,    HM_ALT_R,    HM_GUI_N, KC_M,
		  KC_GRV,  FR_A_GRAVE,    KC_Y,    KC_X,    KC_DOT,    KC_K,                                FR_QUOT,    KC_Q,    KC_G, KC_H,  KC_F, KC_W,
		  XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  QK_REP,               QK_AREP,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
		  LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  KC_LSFT,  LT(SYMB, KC_ENT)
		  ),
  [ERGL] = LAYOUT(
		  KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
		  KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
		  KC_EQL,  HM_GUI_A,    HM_ALT_S,    HM_SFT_D,    HM_CTL_F,    HM_HYP_G,    XXXXXXX,           XXXXXXX, HM_HYP_H,    HM_CTL_J,    HM_SFT_K,    HM_ALT_L,    HM_GUI_SCLN, KC_QUOT,
		  KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
		  XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  QK_REP,               QK_AREP,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
		  LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  KC_LSFT,  LT(SYMB, KC_ENT)
		  ),
  [QWER] = LAYOUT(
		  KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
		  KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
		  KC_EQL,  HM_GUI_A,    HM_ALT_S,    HM_SFT_D,    HM_CTL_F,    HM_HYP_G,    XXXXXXX,           XXXXXXX, HM_HYP_H,    HM_CTL_J,    HM_SFT_K,    HM_ALT_L,    HM_GUI_SCLN, KC_QUOT,
		  KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
		  XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  QK_REP,               QK_AREP,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
		  LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  KC_LSFT,  LT(SYMB, KC_ENT)
		  ),

  [SYMB] = LAYOUT(
		  VRSN,    _______,   _______,   _______,   _______,   _______,   XXXXXXX,           XXXXXXX, _______,  _______,  _______,  _______, _______,  _______,
		  KC_BSLS, FR_GRAVE, KC_LABK, KC_RABK, KC_PMNS, KC_PIPE,  XXXXXXX,           XXXXXXX, KC_CIRC, KC_LCBR, KC_RCBR, KC_DLR,  FR_CIRC, XXXXXXX,
		  XXXXXXX, KC_EXLM,  KC_PAST, KC_PSLS, KC_EQL,  KC_AMPR,  XXXXXXX,           XXXXXXX, KC_HASH, KC_LPRN, KC_RPRN, KC_SCLN, FR_DQUO, XXXXXXX,
		  XXXXXXX, KC_TILD,  KC_PLUS, KC_LBRC, KC_RBRC, KC_PERC,                              KC_AT,   KC_COLN, KC_COMM, KC_DOT,  FR_QUOT, XXXXXXX,
		  XXXXXXX, XXXXXXX,  XXXXXXX, XXXXXXX, _______,           RM_VALU,           RM_TOGG,          _______, XXXXXXX, XXXXXXX, XXXXXXX,  XXXXXXX,
		  RM_HUED, RM_VALD, RM_HUEU, TOGGLE_LAYER_COLOR,_______, _______
		  ),
  [NUMB] = LAYOUT(
		  VRSN,    _______,   _______,   _______,   _______,   _______,   _______,           _______, _______,  _______,  _______,  _______, _______,  _______,
		  _______, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   _______,           _______, _______, KC_7,    KC_8,    KC_9,    KC_PAST, KC_PSLS,
		  _______, KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  _______,           _______, _______, KC_4,    KC_5,    KC_6,    KC_PMNS, KC_PPLS,
		  _______, KC_F11,  KC_F12,  KC_F13,  KC_F14,  KC_F15,                              _______, KC_1,    KC_2,    KC_3,    KC_PDOT, KC_PCMM,
		  EE_CLR,  _______, _______, _______, _______,          _______,           _______,          KC_0, _______, _______,    _______, _______,
		  _______, _______, _______,           _______, _______, _______
		  ),
  [NAVI] = LAYOUT(
		  VRSN,    _______,   _______,   _______,   _______,   _______,   _______,           _______, _______,  _______,  _______,  _______, _______,  QK_BOOT,
		  _______, _______, KC_AT,   KC_LCBR, KC_RCBR, KC_PIPE,  _______,           _______, KC_PSCR,   KC_HOME,    KC_UP,    KC_END,    KC_PGUP, _______,
		  _______, _______, KC_MPRV, KC_MPLY, KC_MNXT, _______,  _______,           _______, _______, KC_LEFT,    KC_DOWN,    KC_RGHT,    KC_PGDN, _______,
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
		  MS_BTN1, MS_BTN3, _______,           _______, MS_BTN3, MS_BTN2
		  ),
};

