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

enum layers {
  BEPO,
  ERGL,
  QWER,
  SYMB,
  NUMB,
  NAVI,
  MOUS,
};

enum custom_keycodes {
  VRSN = SAFE_RANGE,
  FR_DQUO,
  FR_QUOT,
  FR_GRAVE,
  FR_CIRC,
  FR_E_AIGU,
  FR_E_AIGU_CAPS,
  FR_E_GRAVE,
  FR_E_GRAVE_CAPS,
  FR_A_GRAVE,
  FR_A_GRAVE_CAPS,
};

const key_override_t circ_exclamation_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_CIRC, KC_EXLM, 1 << BEPO);
const key_override_t dot_colon_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_DOT, KC_COLN, 1 << BEPO);
const key_override_t comma_semicolon_override = ko_make_with_layers(MOD_MASK_SHIFT, HM_HYP_COMM, KC_SCLN, 1 << BEPO);

// bépo layer
// row 0:
// const key_override_t dquo_one_override = ko_make_with_layers(MOD_MASK_SHIFT, FR_DQUO, KC_1, 1 << BEPO); XXX: managed by a macro
const key_override_t ldaq_two_override = ko_make_with_layers(MOD_MASK_SHIFT, US_LDAQ, KC_2, 1 << BEPO);
const key_override_t rdaq_three_override = ko_make_with_layers(MOD_MASK_SHIFT, US_RDAQ, KC_3, 1 << BEPO);
const key_override_t lprn_four_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_LPRN, KC_4, 1 << BEPO);
const key_override_t rprn_five_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_RPRN, KC_5, 1 << BEPO);
const key_override_t at_six_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_AT, KC_6, 1 << BEPO);
const key_override_t plus_seven_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_PLUS, KC_7, 1 << BEPO);
const key_override_t minus_eight_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_PMNS, KC_8, 1 << BEPO);
const key_override_t slash_nine_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_PSLS, KC_9, 1 << BEPO);
const key_override_t star_zero_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_PAST, KC_0, 1 << BEPO);

// row 1
const key_override_t b_pipe_override = ko_make_with_layers_and_negmods(MOD_BIT_RALT, KC_B, KC_PIPE, 1 << BEPO, MOD_MASK_SHIFT);
const key_override_t b_brkp_override = ko_make_with_layers(MOD_BIT_LSHIFT | MOD_BIT_RALT, KC_B, US_BRKP, 1 << BEPO);

const key_override_t *key_overrides[] = {
  &circ_exclamation_override,
  &dot_colon_override,
  // &quote_question_override,
  &comma_semicolon_override,

  // bépo
  &ldaq_two_override,
  &rdaq_three_override,
  &lprn_four_override,
  &rprn_five_override,
  &at_six_override,
  &plus_seven_override,
  &minus_eight_override,
  &slash_nine_override,
  &star_zero_override,
  &b_pipe_override,
  &b_brkp_override,
};

#define COMBO_REF_DEFAULT QWER

uint8_t combo_ref_from_layer(uint8_t layer){
  switch (get_highest_layer(layer_state)){
  case BEPO: return QWER;
  default: return QWER;
  }
  return layer;  // important if default is not in case.
}

enum combos {
  TO_BEPO, TO_ERGOL, TO_QWERTY,
  TOGGLE_MOUSE,
  BEPO_ESC, QWERTY_ESC,
};

const uint16_t PROGMEM combo_to_bepo[] = {LT(NAVI,KC_BSPC), KC_LSFT, COMBO_END};
const uint16_t PROGMEM combo_to_ergol[] = {LT(NUMB,KC_SPC), LT(SYMB, KC_ENT), COMBO_END};
const uint16_t PROGMEM combo_to_qwerty[] = {KC_DEL, KC_RALT, COMBO_END};
const uint16_t PROGMEM combo_toggle_mouse[] = {KC_Q, KC_R, COMBO_END};

const uint16_t PROGMEM combo_bepo_escape[] = {HM_ALT_R, HM_GUI_N, COMBO_END};
const uint16_t PROGMEM combo_qwe_escape[] = {HM_ALT_L, HM_GUI_SCLN, COMBO_END};

combo_t key_combos[] = {
  // Layers
  [TO_BEPO] = COMBO(combo_to_bepo, TO(BEPO)),
  [TO_ERGOL] = COMBO(combo_to_ergol, TO(ERGL)),
  [TO_QWERTY] = COMBO(combo_to_qwerty, TO(QWER)),
  [TOGGLE_MOUSE] = COMBO(combo_toggle_mouse, TG(MOUS)),
  // Others
  [QWERTY_ESC] = COMBO(combo_qwe_escape, KC_ESC),
  [BEPO_ESC] = COMBO(combo_bepo_escape, KC_ESC),
};

bool combo_should_trigger(uint16_t combo_index, combo_t *combo, uint16_t keycode, keyrecord_t *record) {
  switch (combo_index) {
  case BEPO_ESC:
    if (layer_state_is(BEPO)) {
      return true;
    }
    return false;
  case QWERTY_ESC:
    if (layer_state_is(QWER)) {
      return true;
    }
    return false;
  }
  return true;
}

tap_dance_action_t tap_dance_actions[] = {
};

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

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  uint8_t current_layer = get_highest_layer(layer_state);
  uint8_t mod_state = get_mods();
  if (record->event.pressed) {
    switch (keycode) {
      // FR_DQUO on bépo layout
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
    case FR_DQUO:
      if (record->event.pressed) {
	// if layer is bepo, and shift is on, send ?
	if (IS_LAYER_ON_STATE(current_layer, BEPO) && (get_mods() & MOD_MASK_SHIFT)) {
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(SS_TAP(X_1));
	  set_mods(mod_state);
	} else {
	  SEND_STRING(FR_DQUO_M);
	}
      }
      break;
    case FR_QUOT:
      // if layer is bepo, and shift is on, send ?
      if (IS_LAYER_ON_STATE(current_layer, BEPO) && (get_mods() & MOD_MASK_SHIFT)) {
	del_mods(MOD_MASK_SHIFT);
	SEND_STRING(SS_LSFT(SS_TAP(X_SLASH)));
	set_mods(mod_state);
      } else {
	SEND_STRING(FR_QUOT_M);
      }
      break;
    case FR_GRAVE:
      if (record->event.pressed) {
	SEND_STRING(FR_GRAVE_M);
      }
      break;
    case FR_CIRC:
      if (record->event.pressed) {
	SEND_STRING(FR_CIRC_M);
      }
      break;
    case FR_E_AIGU:
      if (record->event.pressed) {
	if (get_mods() & MOD_MASK_SHIFT) {
	  // É
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(FR_E_AIGU_CAPS_M);
	  set_mods(mod_state);
	} else {
	  // é
	  SEND_STRING(FR_E_AIGU_M);
	}
      }
      break;
    case FR_E_AIGU_CAPS:
      if (record->event.pressed) {
	SEND_STRING(FR_E_AIGU_CAPS_M);
      }
      break;
    case FR_E_GRAVE:
      if (record->event.pressed) {
	if (get_mods() & MOD_MASK_SHIFT) {
	  // 
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(FR_E_GRAVE_CAPS_M);
	  set_mods(mod_state);
	} else {
	  // è
	  SEND_STRING(FR_E_GRAVE_M); 
	}
      }
      break;
    case FR_E_GRAVE_CAPS:
      if (record->event.pressed) {
	SEND_STRING(FR_E_GRAVE_CAPS_M);
      }
      break;
    case FR_A_GRAVE:
      if (record->event.pressed) {
	if (get_mods() & MOD_MASK_SHIFT) {
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(FR_A_GRAVE_CAPS_M);
	  set_mods(mod_state);
	} else {
	  // à
	  SEND_STRING(FR_A_GRAVE_M); 
	}
      }
      break;
    case FR_A_GRAVE_CAPS:
      if (record->event.pressed) {
	SEND_STRING(FR_A_GRAVE_CAPS_M);
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
#define RGB_MATH_OPS 29, 204, 67
#define RGB_QUOTES 204, 202, 29
#define RGB_PUNCTUATION 29, 120, 204
#define RGB_SPECIAL 174, 29, 204

// Shorthand for RGB values - ___ means RGB_OFF
#define ___ {RGB_OFF}

// LED_LAYOUT macro - matches the visual representation of the keyboard LAYOUT
// This makes it easy to see which keys have which colors
#define LED_LAYOUT( \
    k00, k05, k10, k15, k20, k25, k29,           k65, k61, k56, k51, k46, k41, k36, \
    k01, k06, k11, k16, k21, k26, k30,           k66, k62, k57, k52, k47, k42, k37, \
    k02, k07, k12, k17, k22, k27, k31,           k67, k63, k58, k53, k48, k43, k38, \
    k03, k08, k13, k18, k23, k28,                     k64, k59, k54, k49, k44, k39, \
    k04, k09, k14, k19, k24,      k35,           k71,      k60, k55, k50, k45, k40, \
                        k32, k33, k34,           k70, k69, k68 \
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
    [BEPO] = LED_LAYOUT(
        ___,                 {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, ___,                           ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW},
        {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, ___,                           ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW},
        {RGB_GREEN},         {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN},         ___,                           ___,             {RGB_GREEN},         {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN},
        {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},                                                          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},
        ___,                 ___,                 ___,                 ___,                 {RGB_BLUE_LIGHT},                         {RGB_BLUE_LIGHT},              {RGB_BLUE_LIGHT},                     {RGB_BLUE_LIGHT},    ___,                 ___,                 ___,                 ___,
                                                                       {RGB_BLUE_LIGHT},    {RGB_BLUE_LIGHT},    ___,                           ___,             {RGB_BLUE_LIGHT},    {RGB_BLUE_LIGHT}
    ),
    [ERGL] = LED_LAYOUT(
        ___,                 {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, ___,                           ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW},
        {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, ___,                           ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW},
        {RGB_GREEN},         {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN},         ___,                           ___,             {RGB_GREEN},         {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN},
        {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},                                                          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},
        ___,                 ___,                 ___,                 ___,                 {RGB_BLUE_LIGHT},                         {RGB_BLUE_LIGHT},              {RGB_BLUE_LIGHT},                     {RGB_BLUE_LIGHT},    ___,                 ___,                 ___,                 ___,
                                                                       {RGB_BLUE_LIGHT},    {RGB_BLUE_LIGHT},    {RGB_BLUE_LIGHT},              {RGB_BLUE_LIGHT},    {RGB_BLUE_LIGHT},    {RGB_BLUE_LIGHT}
    ),
    [QWER] = LED_LAYOUT(
        ___,                 {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, ___,                           ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW},
        {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, ___,                           ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW},
        {RGB_GREEN},         {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN},         ___,                           ___,             {RGB_GREEN},         {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN_LIGHT},   {RGB_GREEN},
        {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},                                                          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},          {RGB_CYAN},
        ___,                 ___,                 ___,                 ___,                 {RGB_BLUE_LIGHT},                         {RGB_BLUE_LIGHT},              {RGB_BLUE_LIGHT},                     {RGB_BLUE_LIGHT},    ___,                 ___,                 ___,                 ___,
                                                                       {RGB_BLUE_LIGHT},    {RGB_BLUE_LIGHT},    {RGB_GREEN_MEDIA},             {RGB_GREEN_MEDIA},   {RGB_BLUE_LIGHT},    {RGB_BLUE_LIGHT}
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
        ___,                 ___,           ___,           ___,           ___,           ___,            ___,                           ___,             ___,                 ___,                 ___,                 ___,                 ___,              ___,
        {RGB_MATH_OPS},      {RGB_YELLOW},  {RGB_MATH_OPS},{RGB_MATH_OPS},{RGB_YELLOW}, {RGB_YELLOW},   ___,                           ___,             ___,                 ___,                 {RGB_YELLOW},        {RGB_YELLOW},        ___,              {RGB_SPECIAL},
        ___,                 {RGB_YELLOW},  {RGB_MATH_OPS},{RGB_MATH_OPS},{RGB_MATH_OPS},{RGB_YELLOW},   ___,                           ___,             {RGB_YELLOW},        {RGB_YELLOW},        {RGB_GREEN},         {RGB_GREEN},         {RGB_QUOTES},     {RGB_QUOTES},
        ___,                 ___,           ___,           {RGB_GREEN},   {RGB_GREEN},   {RGB_YELLOW},                                                   ___,                 {RGB_PUNCTUATION},   {RGB_PUNCTUATION},   {RGB_PUNCTUATION},   {RGB_PUNCTUATION},___,
        ___,                 ___,           ___,           ___,           ___,                            ___,                           ___,                                  ___,                 ___,                 ___,                 ___,              ___,
                                                                          ___,           ___,            ___,                           ___,             ___,                 ___
    ),
    [NAVI] = LED_LAYOUT(
        {RGB_RED},      ___,        ___,        ___,        ___,        ___,        ___,                           ___,        ___,        ___,        ___,        ___,        ___,        {RGB_RED},
        ___,            ___,        ___,        ___,        ___,        ___,        ___,                           ___,        {RGB_WHITE}, ___,        {RGB_BLUE_LIGHT}, {RGB_PURPLE}, {RGB_PURPLE}, ___,
        ___,            ___,        {RGB_GREEN_MEDIA}, ___,  ___,        ___,        ___,                           ___,        ___,        ___,        {RGB_BLUE_LIGHT}, {RGB_BLUE_LIGHT}, {RGB_PURPLE}, ___,
        ___,            ___,        ___,        {RGB_GREEN_MEDIA}, {RGB_GREEN_MEDIA}, ___,                                   {RGB_PURPLE_VOL}, ___,     ___,        {RGB_BLUE_LIGHT}, ___,        ___,
        ___,            ___,        ___,        ___,        ___,                    {RGB_PURPLE_VOL},              {RGB_PURPLE_VOL},        ___,        ___,        ___,        ___,        ___,
                                                ___,        ___,        ___,                           ___,        ___,        ___
    ),
    [MOUS] = LED_LAYOUT(
        ___,             ___,             ___,             ___,             ___,             ___,             ___,                           ___,             ___,             ___,             ___,             ___,             ___,             ___,
        ___,             ___,             ___,             ___,             ___,             ___,             ___,                           ___,             ___,             {RGB_BLUE_LIGHT}, ___,             ___,             ___,             ___,
        ___,             {RGB_PURPLE},    {RGB_PURPLE},    {RGB_PURPLE},    {RGB_PURPLE},    ___,             ___,                           ___,             ___,             {RGB_BLUE_LIGHT}, {RGB_BLUE_LIGHT}, ___,             ___,             ___,
        ___,             ___,             ___,             ___,             ___,             ___,                                                             ___,             {RGB_BLUE_LIGHT}, ___,             ___,             ___,             ___,
        ___,             ___,             ___,             ___,             ___,                              ___,                           ___,                              ___,             ___,             ___,             ___,             ___,
                                                                            {RGB_PURPLE_VOL},{RGB_PURPLE_VOL},___,                           ___,             {RGB_PURPLE_VOL},{RGB_PURPLE_VOL}
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
//                             │ 32 │ 33 │ 34 │   │ 70 │ 69 │ 68 │
//                             └────┴────┴────┘   └────┴────┴────┘
// Runs constantly in the background, in a loop.
bool rgb_matrix_indicators_user(void) {
  if (rgb_matrix_get_flags() & (LED_FLAG_KEYLIGHT | LED_FLAG_MODIFIER)) {
    uint8_t layer = get_highest_layer(layer_state);
    switch (layer) {
    case BEPO:
    case ERGL:
    case QWER:
    case NUMB:
    case SYMB:
    case NAVI:
    case MOUS:
      rgb_matrix_set_color_all(RGB_OFF);
      set_layer_color(layer);
      break;
    default:
      rgb_matrix_set_color_all(RGB_OFF);
      break;
    }
  }
  return false;
}

void caps_word_set_user(bool active) {
  if (active) {
    rgb_matrix_set_color(40, 255, 255, 255);
  } else {
    rgb_matrix_set_color(40, RGB_OFF);
  }
}
